# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(caret)
library(kernlab)
library(e1071)
library(ISLR)
library(ggplot2)
library(Hmisc)
library(grid)
library(gridExtra)

# datapaths
# main_path <- "C:/Users/arne/DS_Programming_Courses/Coursera/PracticalMachineLearning/week2"
# inpath <- paste(main_path, "data", sep ="/")

# loading data
data(Wage)

# observing data
summary(Wage)

# split into training & test set (eda only done on training set)
set.seed(56789)
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

# get overview over relationships btw. explanatory and explained variable(s)
featurePlot(x=training[,c("age", "education", "jobclass")],
            y=training$wage,
            plot="pairs")

# create plots
qplot(age, wage, data=training) # age vs wage
qplot(age, wage, color=jobclass, data=training) # color by jobcalss to check what distinguishes the two clusters
qq <- qplot(age, wage, color=education, data=training)
qq + geom_smooth(method = 'lm', formula = y~x) # add regression lines for each education group

# creating factors for boxplots
cutWage <- cut2(training$wage, g=3)
table(cutWage)
p1 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot"))
p1 # age boxplots for each wage category
p2 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot", "jitter"))
grid.arrange(p1,p2,ncol=2)

# exploratory tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

# density plots
qplot(wage, color=education, data=training, geom="density")
