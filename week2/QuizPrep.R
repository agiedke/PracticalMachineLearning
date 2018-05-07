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
# first time
# correct
# second time
# incorrect

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
# find cols starting with IL
training_IL <- training[,grepl("IL",names(training))]
training_IL <- training_IL[,-ncol(training_IL)] # last column is called TRAIL_R3
# PCA
str(training_IL)
preProc <- preProcess(training_IL,method="pca",thresh=0.8)
preProc
preProc$numComp # number of components needed for 0.9 % of variation
pc <- predict(preProc,training_IL)


# first time
# answer: 7
# correct

# second time
# question: number of principal components needed for 90 % variation
# answer: 9
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
# subsetting to IL
training_IL <- training[,grepl("IL",names(training))]
training_IL <- training_IL[,-ncol(training_IL)] # last column is called TRAIL_R3
# PCA model
preProc <- preProcess(training_IL,method="pca",thresh=0.8)
trainPC <- predict(preProc,training_IL)
modelFit_pca <- train(x=trainPC, y=training$diagnosis,method="glm")
modelFit_pca
# non-pca model
# ... try re-subset trainng_IL
training_IL <- training[,grepl("IL",names(training))|grepl("diagnosis", names(training))]
training_IL <- training_IL[,-ncol(training_IL)]
#training_IL <- cbind(training_IL, training$diagnosis)
#names(training_IL)[ncol(training_IL)] <- "diagnosis"
modelFit_full <- train(diagnosis ~ ., data=training_IL, method="glm")
modelFit_full
# first time
# answer: Non-PCA:0.75, PCA:0.71
# wrong

# question: use IL + diagnosis -> use PCA with threshold = 80%, apply glm, which one is better
# doesnt add up: pca=71, non-pca = 68
# answer: non-pca=65, pca=72
# correct
