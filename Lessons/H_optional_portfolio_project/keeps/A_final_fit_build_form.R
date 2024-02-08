#' TK
#' Build a lm and webform and save it
#' Feb 2, 2024

# Libs
library(vtreat)
library(devtools)
library(MLmetrics)


# WD
setwd("~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles")

# Custom functions
source_url("https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Lessons/H_optional_portfolio_project/keeps/B_renderRegressionForm_basic.R")


# Data
df <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Lessons/H_optional_portfolio_project/data_collection/33K_2024-01-27_diamonds.csv')

# SAMPLE into 80/20
splitPercent <- round(nrow(df) %*% .8)
totalRecords <- 1:nrow(df)
idx          <- sample(totalRecords, splitPercent)
train        <- df[idx, ]
test         <- df[-idx, ]

# EXPLORE
head(train)
summary(train)
table(train$diamondType)
plot(density(train$diamondPrice))
boxplot(train$diamondPrice)


### MODIFY
# 1. Drop the column that has no variation
train$diamondType <- NULL

# 2. Drop outlier price diamonds defined by boxplot
# Define your interquartile range - remove 25% lowest values, remove 25% highest values, leaving the 50% in the middle
# Keep in mind we're removing them from the train not test set so we would assume during assessment that our model will degrade for outliers that remain.
outlierIndex <- boxplot.stats(train$diamondPrice)$out
train <- train[-outlierIndex,]

# 3. Now apply a design treatment function from vtreat
informativeFeatures <- names(train)[1:13]
target <- names(train)[14]
plan <- designTreatmentsN(train, 
                          varlist = informativeFeatures,
                          outcomename = target)

# Prepare both the train and test sets
treatedTrain <- prepare(plan, train)
treatedTest <- prepare(plan, test)

# Since this is a model where a user is making selections we need to drop the engineered variables 
drops <- grep('_cat|_isBAD', names(treatedTrain))
treatedTrain <- treatedTrain[,-drops]

### MODEL
fit <- lm(diamondPrice ~., treatedTrain)
bestFit <- step(fit, direction = 'backward')

### ASSESS
summary(bestFit)
trainPreds <- predict(bestFit, treatedTrain)
testPreds  <- predict(bestFit, treatedTest)

# Train RMSE
MLmetrics::RMSE(y_pred = trainPreds, y_true = treatedTrain$diamondPrice)

# Test RMSE
MLmetrics::RMSE(y_pred = testPreds, y_true = treatedTest$diamondPrice)

### Deploy as webpage
# WORKS
renderRegressionForm(lm_obj      = bestFit, 
                     original_df = treatedTrain, 
                     fileName    = 'test3.html')


renderRegressionForm_CSS(lm_obj      = bestFit, 
                      original_df = treatedTrain, 
                      fileName    = 'test3.html')

# End