rm(list=ls())
dev.off()

# libraries
library(ggplot2)
library(caret)
library(rattle)

# data
data(iris)

# explore data
names(iris)
table(iris$Species)

# creating training and test set
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

# exploring test set
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)


# building tree
modelFit <- train(Species~., method="rpart", data=training)
print(modelFit$finalModel)


# plotting tree
plot(modelFit$finalModel, uniform=T,
     main="Classification Tree")
text(modelFit$finalModel, use.n = T, all=T, cex=.8)
fancyRpartPlot(modelFit$finalModel) # using rattle package

# predicting new variables
predict(modelFit, newdata=testing)
