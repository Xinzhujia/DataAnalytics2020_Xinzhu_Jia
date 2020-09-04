#Data exploration for Boston Dataset
#set the working directory
setwd("/Users/apple/Desktop/Data Analytics/R")
#install package
install.packages("MASS")
library(MASS)
attach(Boston)
?Boston
help(Boston)

#explore Boston data 
head(Boston)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston)
summary(Boston$crim)

#install package
install.packages("ISLR")
library(ISLR)
data("Auto")

#Boston data exploration
head(Auto)
help("Auto")
head(Auto,10)
fivenum(Auto$mpg)
boxplot(Auto$weight)
mean(Auto$weight)
median(Auto$weight)

#import EPI data
help("read.csv")
data <- read.csv("EPI_data.csv", header = TRUE)
data

#EPI data exploration
dim(data)
head(data)
names(data)
summary(data)
fivenum(data$EPI)

#visualize EPI column specifically
hist(data$EPI)
boxplot(data$EPI)

#explore NA in the dataset
help("is.na")
is.na(data)
sum(is.na(data))
