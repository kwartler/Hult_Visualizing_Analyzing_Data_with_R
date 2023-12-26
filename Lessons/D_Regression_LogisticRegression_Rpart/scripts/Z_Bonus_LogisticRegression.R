#' Ted Kwartler
#' Date: Feb 26, 2023
#' Goal: Build a default model using logistic regression

# WD
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Libraries
library(ggplot2)
library(ggthemes)
library(vtreat)
library(MLmetrics)
library(ROSE)

# Data
ccData <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/UCI_Credit_Card.csv')

idx <- sample(1:nrow(ccData), 30000*0.8)

# Sample
training <- ccData[idx,]
validation <- ccData[-idx,]

# Explore
head(training)
summary(training)
names(training)
table(training$default.payment.next.month)
ggplot(training, aes(x= MARRIAGE)) + geom_bar() + theme_hc()
ggplot(training, aes(x= SEX)) + geom_bar() + theme_hc()
ggplot(training, aes(x= default.payment.next.month)) + 
  geom_bar() + theme_hc()


# Modify
informativeFeatures <- names(ccData)[2:24]
targetVariable      <- names(ccData)[25]
successClass        <- '1'

plan <- designTreatmentsC(training, 
                          informativeFeatures, 
                          targetVariable, 
                          successClass)

# In reality you would partition the prepData as shown previously
treatedTrain <- prepare(plan, training)
treatedValidation <- prepare(plan, validation)

#### Unbalanced Y,over sampling data to rebalance Y lets the algo find the "signal" easier
# ROSE - Random Over Sample Examples
#trainingRose <- ROSE(default.payment.next.month~., treatedTrain)
#table(trainingRose$data$default.payment.next.month)
# You could use trainingRose$data and treatedTrain to build two models then compare choosing the more accurate model (assumes balanced classification costs)

# Model
fit <- glm(default.payment.next.month ~., treatedTrain, family = 'binomial')
summary(fit)

# Assess
trainPreds <- predict(fit,  treatedTrain, type='response')
testPreds  <- predict(fit,  treatedValidation, type='response')

# Classify 
cutoff      <- 0.5
trainClasses <- ifelse(trainPreds >= cutoff, 1,0)

# Organize w/Actual
trainResults <- data.frame(ccData$ID[idx],
                           actual  =treatedTrain$default.payment.next.month,
                           probablity = trainPreds,
                           classes = trainClasses)
head(trainResults)

# Get a confusion matrix
(confMat <- ConfusionMatrix(trainResults$classes, trainResults$actual))
Accuracy(trainResults$classes, trainResults$actual)

#when you look at the confusion matrix of fit from data=trainingRose$data the accuracy goes down but you capture more 1 true/1 predicted.  So its finding signal easier for the success class.

# End