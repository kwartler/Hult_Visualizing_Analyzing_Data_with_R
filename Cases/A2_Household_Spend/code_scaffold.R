#' Author
#' Jan 27, 2024
#' Purpose initialize the submission for A2 using lm and rpart

# libraries
library(rpart)
library(caret)
library(plyr)
library(dplyr)
library(MLmetrics)
library(vtreat)

# WD
setwd("~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles")

# data
allTrainingFiles <- list.files(path = '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Cases/A2_Household_Spend/studentTables',
                               pattern = 'training',
                               full.names = T)

# Load the files and arrange them with a left join
allTrainingDF <- lapply(allTrainingFiles, read.csv)
allTrainingDF <- join_all(allTrainingDF, by='tmpID', type='left')

### You will need to repeat this for the testing data and prospect data files to get a single data frame for each.  You can ask GPT to explain this code to you if you don't know where to change it.

# SAMPLE
### Using the training data, create a validation set.  Remember you already have a test set in the files that are separate but you should still subset this set 
train      <- '...'
validation <- '...'

# EXPLORE
### Perform robust exploratory data analysis, drop any columns that you think don't make sense, or are unethical to use in use case.  You can build visuals, tables, summaries and explore the data's overall integrity.

# MODIFY 
### Using the training, create a design treatment plan.
#1 Identify the informative features (x variables)
#2 Identify the target variable (y variable)

# All column names, after EXPLORE you should know which variables you want to use
names(allTrainingDF) # all variables

# Example of some variables and building a plan
informartiveFeatures <- c('ResidenceHHGenderDescription', 'EthnicDescription', 'DogOwner','Gender')
targetName <- 'yHat'
plan <- designTreatmentsN(dframe      = train, 
                          varlist     = informartiveFeatures,
                          outcomename = targetName)

# Apply the plan to all sections of the data
treatedTrain <- prepare(plan, train)
treatedValidation <- prepare(plan, validation) # this is the subset of data from line 30
treatedTest <- prepare(plan, testData) # this is the data set from repeating the read.csv section but with the test CSV files
treatedProspects <- prepare(plan, prospectData) #this is the data set from repeating read.csv but with the prospect CSV files

# MODEL
### You should be able to fit a linear model and you should challenge yourself to research and build additional algorithm types
fit <- lm(yHat~., treatedTrain)

# Other model 1
# Other model 2

# ASSESS
### Make predictions
linearTrainPredictions      <- predict(fit, treatedTrain)
linearValidationPredictions <- predict(fit, treatedValidation)
 

# Next, calculate the RMSE for these sections.  Look for consistency.  At this point you could go back and adjust the model by adding or subtracting variables.  

# Once you are done get predictions for the test set
linearTestPredictions       <- predict(fit, treatedTest) 

# Compare the training, validation and test set RMSE.  Look for consistency.
# Repeat these steps and calculate the RMSEs for any other models you created.

# Select the best model you have based on the RMSE score in the test set that is also fairly consistent among other data sections.

# Using the best possible model make predictions on the prospect set.
prospectPredictions <- predict(bestModel, treatedProspects)

# Column bind the predictions to the prospect CSV; finish the case submissions. 

# End