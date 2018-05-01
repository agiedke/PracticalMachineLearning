# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(caret)
library(kernlab)
# library(e1071)
# library(ISLR)
# library(ggplot2)
# library(Hmisc)
# library(grid)
# library(gridExtra)

# loading data & partition
data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="", xlab="ave. capital run length") # data very skewed
mean(training$capitalAve)
sd(training$capitalAve)

# standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve) # standardizing CapAve
mean(trainCapAveS) # mean=0
sd(trainCapAveS) # sd=1

# standardizing test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve) # using params from train set!!!
mean(testCapAveS) # not exactly 0
sd(testCapAveS) # not exactly 1

# standardizing with preProcess function
