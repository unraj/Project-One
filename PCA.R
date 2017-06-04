rm(list=ls())
#setting working directory
setwd("C:\\Users\\Uday\\Desktop\\insofe\\project\\dataset")
#Import train data
train <- read.csv('train.csv',header=T,na.strings=c(""))
train_data = train[,-c(562,563)]
str(train_data)
dim(train_data)

#test data set
test <- read.csv('test.csv',header=T)
test_data= test[,-c(562,563)]
str(test_data)
dim(test_data)

#PCA model on train data
library(class)
pca<-prcomp(train_data)
summary(pca)

pca_train<-as.data.frame(predict(pca,train_data))
pca_test<-as.data.frame(predict(pca,test_data))

preds=knn(pca_train[,1:80],pca_test[,1:80], train$Activity, k = 6)
a=table(test$Activity,preds)
a

#Confusion matrix
library(caret)
confusionMatrix(preds,test$Activity)

