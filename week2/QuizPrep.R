# remove all data in memory
rm(list=ls())
dev.off()

# Q1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
#
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
head(diagnosis)
# correct

# Q2
rm(list=ls())
dev.off()
#
library(AppliedPredictiveModeling)
library(ggplot2)
library(caret)
library(Hmisc)
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
#
str(training)
#base plot
with(training, plot(CompressiveStrength, type='p'))
#ggplot
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Cement)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$BlastFurnaceSlag)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$FlyAsh)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Water)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Superplasticizer)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$CoarseAggregate)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$FineAggregate)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Age)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$CompressiveStrength)
# grouping values
nr_groups <- 4
for(name in names(training)[1:8]){
  #print(head(cut2(training[,name],g=nr_groups)))
  training[,name] <- cut2(training[,name],g=nr_groups)
}
#ggplot
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Cement)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$BlastFurnaceSlag)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$FlyAsh)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Water)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Superplasticizer)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$CoarseAggregate)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$FineAggregate)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$Age)
qplot(seq_along(training$CompressiveStrength),training$CompressiveStrength, color=training$CompressiveStrength)
# answer: non-random pattern not explained by any predictor
# correct

# Q3
rm(list=ls())
dev.off()
#
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
str(training)
with(training, hist(Superplasticizer))
with(training, hist(log(Superplasticizer)))
summary(training$Superplasticizer)
# answer: many 0 values -> -Inf
# correct

# Q4
rm(list=ls())
dev.off()
#
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#
#...
# answer: 7
# correct

# Q5
rm(list=ls())
#
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#
# ...
# answer: Non-PCA:0.75, PCA:0.71
# wrong