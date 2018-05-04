# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(caret)
# library(kernlab)
# library(RANN)
# library(e1071)
library(ISLR)
# library(ggplot2)
# library(Hmisc)
# library(grid)
# library(gridExtra)
library(splines)

# load data
data(Wage)

# partition data
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# add covariates
table(training$jobclass)
dummies <- dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training)) # use predict function in combi with dummyVars function to convert factors to dummies

# removing zero covariates
nsv <- nearZeroVar(training,saveMetrics=T)
nsv # region has almost no variation

# spline basis
bsBasis <- bs(training$age, df=3)
bsBasis # transforms variable in order to fit non-linear curve (in this case 3rd degree polynomial)
lm1 <- lm(wage ~ bsBasis, data=training)
# plotting results
plot(training$age, training$wage, pch=19,cex=0.5)
points(training$age, predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
# applying on test set
head(predict(bsBasis, age=testing$age)) # apply same procedure as on training set! use predict in combination with bsBasis!
