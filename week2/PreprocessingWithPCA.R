# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(caret)
library(kernlab)
# library(RANN)
# library(e1071)
# library(ISLR)
# library(ggplot2)
# library(Hmisc)
# library(grid)
# library(gridExtra)
# library(splines)

# load data
data(spam)

# partition data
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# checking for high correlating predictors
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M>0.8,arr.ind=T) # display variables with high correlation
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

# transformation example: rotating the plot
x <- 0.71*training$num415 + 0.71*training$num857
y <- 0.71*training$num415 - 0.71*training$num857
plot(x,y)

# using pca function
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation

# application to spam data example
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1", ylab="PC2")

# pca with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

# preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
#modelFit <- train(training$type ~ .,method="glm",data=trainPC) # doesnt run due to caret update
modelFit <- train(x=trainPC, y=training$type,method="glm")
#
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))
# alternative PCs (not working)
modelFit <- train(training$type ~.,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))
