#Clean workspace
rm(list=ls())
#Setting working directory
setwd("C:\\Users\\Uday\\Desktop\\insofe\\project\\dataset")
#train data set
train<- read.csv('train.csv',header=T,na.strings=c(""))
#Remove subject and activity from variables
train_data = train[,-c(562,563)]
str(train_data)
dim(train_data)

#test data set
test <- read.csv('test.csv',header=T)
#Remove subject and activity from variables
test_data= test[,-c(562,563)]
str(test_data)
dim(test_data)

#Target variable
train_target<-train$Activity
test_target<-test$Activity

#RandomForest model building

library(randomForest)

x=data.matrix(train_data)

y=train_target
train_data_rf<- randomForest(x,y, data=train_data, keep.forest=TRUE, ntree=30,mtry=3)
print(train_data_rf)

library(caret)
train_data_rf$predicted
train_data_rf$importance
#Variable importance plot
varImpPlot(train_data_rf)

#Predict on test data
pred<-predict(train_data_rf,test_data)
#confusion matrix
confusionMatrix(pred,test_target)



