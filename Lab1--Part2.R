EPI_data <- read.csv('EPI_data.csv')
dim(EPI_data)
names(EPI_data)
head(EPI_data)
View(EPI_data)
summary(EPI_data)

#qq plot for AIR_H variable 

plot(ecdf(EPI_data$AIR_H),do.points = FALSE, verticals = TRUE)
par(pty='s')
qqnorm(EPI_data$AIR_H);qqline(EPI_data$AIR_H)
hist(EPI_data$AIR_H,seq(0.,100.,2.))

#qq plot for water_H variable 
plot(ecdf(EPI_data$WATER_H),do.points = FALSE, verticals = TRUE)
par(pty='s')
qqnorm(EPI_data$WATER_H);qqline(EPI_data$WATER_H)
hist(EPI_data$WATER_H,seq(0.,100.,2.))

boxplot(EPI_data$AIR_H,EPI_data$WATER_H)


