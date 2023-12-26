#' Author: Ted Kwartler
#' Date: Feb 24, 2023
#' Purpose: newCar Toyota Corolla Backward step Variable Regression
#'
# Options
options(scipen=999)

# Libs
library(vtreat)

# SetWD
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Data
cars <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/newCars.csv')

# drop geo and text for this example
cars$state       <- NULL
cars$city        <- NULL
cars$allFeatures <- NULL

# Target Leakage!  The monthly paymenbt is derived from the y variable directly!
cars$monthlyPayment <- NULL

# Partitioning 20% test set
splitPercent <- round(nrow(cars) %*% .8)

set.seed(2017)
idx      <- sample(1:nrow(cars), splitPercent)
trainSet <- cars[idx, ]
testSet  <- cars[-idx, ]

#PreProcessing
dataPlan     <- designTreatmentsN(cars, names(cars)[6:25], 'listPrice')
treatedTrain <- prepare(dataPlan, trainSet)
treatedTest  <- prepare(dataPlan, testSet)

# Fit
fit <- lm(listPrice ~ ., treatedTrain)

# Step
backFit <- step(fit,direction = 'backward', trace = 5)
saveRDS(backFit,'~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/DD1/E_Mar9/data/backFit.rds')
summary(backFit)

# End