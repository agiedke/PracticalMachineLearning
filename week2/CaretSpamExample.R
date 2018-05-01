# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(caret)
library(kernlab)
library(e1071)

# datapaths
# main_path <- "C:/Users/arne/DS_Programming_Courses/Coursera/PracticalMachineLearning/week2"
# inpath <- paste(main_path, "data", sep ="/")

# loading data
data(spam)

# splitting data into training and test set
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# fitting model
set.seed(32343)
modelFit <- train(type~.,data=training,method="glm")
modelFit
modelFit$finalModel # returns parameters

# evaluating model
predictions <- predict(modelFit, newdata=testing) # storing predictions
predictions
confusionMatrix(predictions, testing$type)
