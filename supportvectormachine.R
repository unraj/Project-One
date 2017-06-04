rm(list=ls())
#setting working directory
setwd("C:\\Users\\Uday\\Desktop\\insofe\\project\\dataset")
#Import train data
train_data <- read.csv('train.csv',header=T,na.strings=c(""))
#Import test data
test_data <- read.csv('test.csv',header=T,na.strings=c(""))

#Exploratory data analysis
class(train_data)
class(test_data)

dim(train_data)
dim(test_data)

Names <- names(train_data)
#Features selection  
#we choose  variable means to reduce number of features and removes the feature "subject"
#this led us to 53 features instead of 563  


idxTrain0 = grep("[M|m]ean",Names)
train0 = train_data[,c(idxTrain0,563)]
test0 = test_data[,c(idxTrain0,563)]

#svm model with tuning 
library(e1071)
library(parallelSVM)
svm.fit = parallelSVM(Activity~. ,
                      samplingSize = 0.4,
                      type = "C-classification",
                      seed = 25,
                      cross = 5,
                      cost = 50,
                      data = train0)
summary(svm.fit)


#Prediction on test data
preds = predict(svm.fit,test0)

#Results
#Classification accuracy
mydata<-sprintf("Classification  accuracy : %1.2f%s",100*mean(preds == test0$Activity),"%")
#checking out misclassification
mydata1<-sprintf("misclassification : %1.2f%s",100*(1 -  mean(preds == test0$Activity)),"%")

str(mydata)

#Svm model tuning with cost and gamma
svm_model_tune <- svm(Activity ~ ., data=train0, kernel="radial", cost=3, gamma=0.5)
summary(svm_model_tune)
#Prediction on test data
preds1 = predict(svm_model_tune,test0)

library(caret)
confusionMatrix(preds,test0$Activity)

svm_model_tune$kernel


