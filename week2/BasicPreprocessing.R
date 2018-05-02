# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(caret)
library(kernlab)
library(RANN)
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
preObj <- preProcess(training[,-58],method=c("center","scale")) # standardizing all but 58th variable (dependent var)
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
# applying to test set
rm(testCapAveS)
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# Using preProcess as argument
set.seed(32343)
modelFit <- train(type~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit

# Box-Cox transforms
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapeAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapeAveS)
qqnorm(trainCapAveS)

# Data Imputation
# making some values na
set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1 # creating index vector for NA replacement
training$capAve[selectNA] <- NA # replacing 5% of values with NA
# Imputing and standardizing
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve
# standardizing true values for comparison
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
# compare
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])
