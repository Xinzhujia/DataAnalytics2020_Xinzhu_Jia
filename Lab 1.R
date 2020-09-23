# Dataframe 
days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
help('data.frame')
RPI_Weather_Week <- data.frame(days, temp, snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')]
RPI_Weather_Week[1:5,'temp']
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
RPI_Weather_Week[dec.snow,]

empty.Dataframe <- data.frame()
v1 <- 1:10
v1
v2 <- letters[1:10]
df <- data.frame(v1,v2)
df
help(write.csv)
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

# EPI data
setwd('/Users/apple/Desktop/Data Analytics/R script')
EPI_data <- read.csv("2010EPI_data.csv", skip = 1, header = T)
head(EPI_data)
dim(EPI_data)
summary(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]

#distribution
summary(EPI)
fivenum(EPI)
stem(EPI)

hist(EPI,seq(30.,95.,1.0),prob = TRUE)
hist(EPI)
help(lines)
lines(density(EPI,na.rm = TRUE, bw = 1.0))
rug(EPI)

#cumulative density function
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)

par(pty = 's')
qqnorm(EPI)
qqline(EPI)
#Simulated data from t-distribution:
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
#Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(EPI,DALY)

#conditional filter 
View(EPI)
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
Eland
hist(Eland)
