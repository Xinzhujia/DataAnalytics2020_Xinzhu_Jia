#import dataset
df <- read.csv("monroe-county-crash-data2003-to-2015.csv",na.strings = "", stringsAsFactors = T)
dim(df)
head(df)
names(df)
summary(df)
str(df)
levels(df$Collision.Type)
levels(df$Injury.Type)
levels(df$Weekend.)
levels(df$Primary.Factor)

#remove four unnecessary columns
df_2 <- df[-c(1,10,11,12)]
levels(df_2$Primary.Factor)
levels(df_2$Collision.Type)
levels(df_2$Injury.Type)
levels(df_2$Weekend.)

#data cleaning 
#delete rows that has missing value or level
nrow(df_2[df_2$Weekend.=="",])
nrow(df_2[df_2$Collision.Type=="",])
nrow(df_2[df_2$Primary.Factor=="",])
df_3 <- df_2[!(df_2$Weekend.=="" | df_2$Collision.Type==""|df_2$Primary.Factor==""),]
sapply(df_3, function(x) sum(is.na(x)))
df_4<- na.omit(df_3)
dim(df_4)
summary(df_4)
str(df_4)

# bar graph of counts
library(ggplot2)
Count_Col <- ggplot(df_4, aes(x=Collision.Type)) + geom_bar( )
print(Count_Col + labs(x = "Collision Type"))
Count_Injury <- ggplot(df_4, aes(x=Injury.Type)) + geom_bar()
print(Count_Injury + labs(x = "Injury Type"))
Count_weekend <- ggplot(df_4, aes(x=as.factor(Weekend.))) + geom_bar( width = 0.4)
print(Count_weekend + labs(x = "Weekend"))
Count_weekend <- ggplot(df_4, aes(x=as.factor(Year))) + geom_bar( width = 0.4)
print(Count_weekend + labs(x = "Year"))
Count_weekend <- ggplot(df_4, aes(x=as.factor(Month))) + geom_bar( width = 0.4)
print(Count_weekend + labs(x = "Month"))
Count_weekend <- ggplot(df_4, aes(x=as.factor(Day))) + geom_bar( width = 0.4)
print(Count_weekend + labs(x = "Day"))
Count_weekend <- ggplot(df_4, aes(x=as.factor(Hour))) + geom_bar( width = 0.4)
print(Count_weekend + labs(x = "Hour"))
plot1 <- ggplot(df_4, aes(x=Collision.Type,fill=Injury.Type))  + 
  geom_bar()+scale_fill_brewer(palette = "Pastel2")+geom_text(aes(label=car_count), position = position_stack(vjust= 0.5),colour = "white", size = 5)
plot2 <- ggplot(df_4, aes(x=Injury.Type="Fatal",fill=Collision.Type))  + 
  geom_bar()+ scale_fill_brewer(palette = "Pastel2")
print(plot2)
head(df_4)
str(df_4)
summary(df_4)

#1.Modeling--Random Forest
#transform variables 
df_4$Day <- factor(df_4$Day)
df_4$Day <- factor(df_4$Day, levels = c(1:7), 
                             labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
df_4$Year <- factor(df_4$Year)
df_4$Month <- factor(df_4$Month)
df_4$Month <- factor(df_4$Month, levels = c(1:12), 
                   labels = c("Jan", "Fri", "Mar", "Apr", "May", "Jun", "Junl",'Aug','Sep','Oct','Nov','Dec'))
df_4$Hour <- factor(round(df_4$Hour/100))
str(df_4)
head(df_4)

# Time and Collision type
df_5<- df_4 [, c(1:6)]
head(df_5)
#Partition the data
library('randomForest')
set.seed(1)
# Partitioning into training (60%) and validation (40%)
train <- sample(c(1:dim(df_5)[1]), dim(df_5)[1]*0.6)  
train.df <- df_5[train, ]
valid.df <- df_5[-train, ]
summary(train.df)
summary(valid.df)
dim(valid.df)
levels(train.df$Collision.Type)
RF <- randomForest(Collision.Type ~., data = train.df, ntree = 500, importance=TRUE)
RF
RF2 <- randomForest(Collision.Type ~., data = train.df, importance=TRUE)
RF2

# attempt 2
df_5<- df_4 [, c(1:7)]
#Partition the data
library('randomForest')
set.seed(1)
# Partitioning into training (60%) and validation (40%)
train <- sample(c(1:dim(df_5)[1]), dim(df_5)[1]*0.6)  
train.df <- df_5[train, ]
valid.df <- df_5[-train, ]
RF3 <- randomForest(Injury.Type ~., data = train.df, ntree = 500,mtry = 2, importance=TRUE)
RF3

# find the best mtry 
n <- ncol(train.df) -1
errRate <- c(1)
for (i in 1:n){
  m <- randomForest(Injury.Type ~., data = train.df,mtry=i,ntree=100)  
  err<-mean(m$err.rate)  
  errRate[i] <- err}
m= which.min(errRate)  
print(m)

#predict with RF model
predict_RF <- predict(RF3,train.df,type = 'class')
table(predict_RF,train.df$Injury.Type)
predict_RF_Valid <- predict(RF3,valid.df,type = 'class')
table(predict_RF_Valid,valid.df$Injury.Type)
#
importance(RF3)
varImpPlot(RF3)

#2.Modeling--Logistic Regression
#create reference categories
str(df_5)
df_5$Year <- relevel(df_5$Year, ref = "2003")
df_5$Month <- relevel(df_5$Month, ref = "Jan")
df_5$Day<- relevel(df_5$Day, ref = "Mon")
df_5$Weekend.<- relevel(df_5$Weekend., ref = "Weekday")
df_5$Collision.Type <- relevel(df_5$Collision.Type, ref = "1-Car")
df_5$Hour <- relevel(df_5$Hour, ref = "0")

# Use set.seed() to get the same partitions when re-running the R code
set.seed(1)
# Partitioning into training (60%) and validation (40%)
train.index <- sample(c(1:dim(df_5)[1]), dim(df_5)[1]*0.6)
train.df_5 <- df_5[train.index, ]
valid.df_5 <- df_5[-train.index, ]

# run logistic model, and show coefficients and odds
lgm <- glm(Injury.Type ~ Hour+Collision.Type, data = train.df_5, family = "binomial")
summary(lgm)
round(data.frame(summary(lgm)$coefficients, odds = exp(coef(lgm))), 5)

#Model Performance
# Use predict() with type = "response" in compute predicted probabilities
pred <- predict(lgm, valid.df_5, type="response")
hist(pred)
pred.bin <- ifelse(pred > 0.5, 1, 0)


#3.Modeling--Decision tree
str(df_4)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
set.seed(1)
# Partitioning into training (60%) and validation (40%)
train.index <- sample(c(1:dim(df_4)[1]), dim(df_4)[1]*0.6)  
train.df_4 <- df_4[train.index, ]
valid.df_4 <- df_4[-train.index, ]

# Create a full-grown tree
deeper.ct <- rpart(Injury.Type ~Year+Month+Day+Hour+Collision.Type, data = train.df_4, method = "class", cp = 0, minsplit = 1)
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

# Prune by lower cp
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
# Count the number of terminal nodes in the pruned tree
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, , under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))  

# Confusion matrix
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
pred <- predict(pruned.ct, valid.df_4, type = "class")
confusionMatrix(pred, valid.df_4$Injury.Type)