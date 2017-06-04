rm(list=ls())
#setting working directory
setwd("C:\\Users\\Uday\\Desktop\\insofe\\project\\dataset")
#Import train data
train <- read.csv('train.csv',header=T,na.strings=c(""))
#Removing subject and target variable from train data
train_data = train[,-c(562,563)]
str(train_data)
dim(train_data)

#Import test data
test <- read.csv('test.csv',header=T)
#Removing subject and target variable from test data
test_data= test[,-c(562,563)]
str(test_data)
dim(test_data)
summary(train_data)


#### Decision Tree Model on train data
library(C50)
DecisionTree <- C5.0(train$Activity~.,data = train_data)
summary(DecisionTree)
DecisionTree

#Prediction on test data
pred_test <- predict(DecisionTree,test_data)
pred_test

library(caret)
#Confusion matrix for accuracy, sensitivity and specificity
confusionMatrix(test$Activity,pred_test)



library(rpart)
DecisionTree1 <- rpart(train$Activity~.,data=train_data,method = "class")
summary(DecisionTree1)


#Tuning the parameters 

mytree <- rpart(train$Activity~.,data=train_data,method = "class", minsplit = 4, minbucket = 3)
mytree

#Predicting on test data after tuning
dt_test <- predict(mytree,test,type = "class")
nrow(dt_test)
dt_test

#Confusion matrix for accuracy, sensitivity and specificity
conf_mat1 <- confusionMatrix(dt_test,test$Activity)
conf_mat1
