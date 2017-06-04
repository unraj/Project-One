rm(list=ls())
## Load libraries
library(FSelector) 
library(ggplot2)

## Importing data
setwd("C:\\Users\\Uday\\Desktop\\insofe\\project\\dataset")
#Train data
train <- read.csv('train.csv',header=T,na.strings=c(""))
#Test data
test <- read.csv('test.csv',header=T,na.strings=c(""))

## Exploratory Visualization
train$Partition = "Train"
test$Partition = "Test"

all = rbind(train,test)


all$Partition = as.factor(all$Partition)

qplot(data = all, x = Activity)
#train data
qplot(data = train, x = Activity)
#test data
qplot(data = test , x = Activity)
#Train and test data
qplot(data = all , x = subject, fill = Activity)

p <- ggplot(all, aes(x=subject, y = Activity)) + geom_boxplot ()
plot(all$Activity)


sub1 <- subset(all, subject == 1)

par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
plot(sub1[, 1], pch=19, col = sub1$Activity, ylab = names(sub1)[1])
plot(sub1[, 2], pch=19, col = sub1$Activity, ylab = names(sub1)[2])
plot(sub1[, 3], pch=19, col = sub1$Activity, ylab = names(sub1)[3])  
plot(sub1[, 2], pch=19, col = all$Activity, ylab = names(all)[2])   # All subjects

legend("bottomright", legend = unique(sub1$Activity), col = unique(sub1$Activity), text.font = 4, 
       pch = 1, cex=0.64)

