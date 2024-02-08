#' TK
#' Build a lm and webform and save it
#' Feb 2, 2024

# Libs
library(________) #variable treatment
library(________) #see if you can find the library that has the function source_url
library(________) #machine learning metrics


# WD
setwd("_____________________")

# Custom functions
source_url("https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Lessons/H_optional_portfolio_project/keeps/B_renderRegressionForm_basic.R")
source_url('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Lessons/H_optional_portfolio_project/keeps/C_renderRegressionForm_CSS.R')


# Data
df <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Lessons/H_optional_portfolio_project/data_collection/33K_2024-01-27_diamonds.csv')

# SAMPLE into 80/20
#
#
#
train        <- df[idx, ]
test         <- df[-idx, ]

# EXPLORE
# Look at the first 6 train rows
# Summarize the train object
# Tabluate the diamondType
# Create a density plot for diamondPrice
# Create a box plot of diamondPrice.  Does this show any outliers? 


### MODIFY
# 1. Drop the column that has no variation (diamondType)
#

# 2. Drop outlier price diamonds defined by boxplot
# Define your interquartile range - remove 25% lowest values, remove 25% highest values, leaving the 50% in the middle
# Keep in mind we're removing them from the train not test set so we would assume during assessment that our model will degrade for outliers that remain.
outlierIndex <- boxplot.stats(train$diamondPrice)$out
train <- train[-outlierIndex,]

# 3. Now apply a design treatment function from vtreat

informativeFeatures <- '' #first 13 column names 
target <- '' #14th column name
plan <- designTreatmentsN(_____, 
                          varlist = __________,
                          outcomename = _______)

# Prepare both the train and test sets
treatedTrain <- ______(______, ______)
treatedTest <- ______(______, ______)

# Since this is a model where a user is making selections we need to drop the engineered variables 
drops <- grep('_cat|_isBAD', names(treatedTrain))
treatedTrain <- treatedTrain[,-drops]

### MODEL
# Fit a lm() with formula diamondPrice ~. using the correct data partition
fit <- lm(____, ____)
bestFit <- _________ # Stepwise backward direction to down select the variables

### ASSESS
# Summarize the bestFit
trainPreds <- ____________________________ # get predictions on treatedTrain
testPreds  <- ____________________________ # get predictions on treatedTest

# Train RMSE
# Get the RMSE for the treatedTrain with bestFit predictions

# Test RMSE
# Get the RMSE for the treatedTest with bestFit predictions

### Deploy as webpage
# WORKS
renderRegressionForm(lm_obj      = bestFit, 
                     original_df = treatedTrain, 
                     fileName    = 'basicPrototype.html')


renderRegressionForm_CSS(lm_obj      = bestFit, 
                      original_df = treatedTrain, 
                      fileName    = 'cssPrototype.html')

# End