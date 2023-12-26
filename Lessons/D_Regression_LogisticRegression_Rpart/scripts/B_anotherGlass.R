#' Author: Ted Kwartler
#' Date: 2-19-2023
#' Purpose: Partitioning Schema
#' 

# Libs
library(vtreat)

# Data
wine <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/Wine.csv')

# Train 90%/Test 10% Partitioning
splitPercent  <- round(nrow(wine) %*% .9)
totalRecords <- 1:nrow(wine)
totalRecords
idx          <- sample(totalRecords, splitPercent)
head(idx,15)

# Remember its row, then columns with the indexing operation.  Here you use the vector of numbers in the "row" position to create a training set and the minus for test set.
trainSet <- wine[idx, ]
testSet  <- wine[-idx, ]

# Dimensions
dim(trainSet)
dim(testSet)

## Now you can train a model on the trainSet and review realistic results on the testSet

# Start over
rm(list=c('trainSet','testSet','splitPercent','totalRecords','idx'))


# Train 50%/Validation 40% /Testing 10%
trainPercentRows      <- round(nrow(wine) %*% .5)
trainPercentRows
validationPercentRows <- round(nrow(wine) %*% .4)
validationPercentRows

# Sample index for training
trainIdx <- sample(1:nrow(wine), trainPercentRows)
head(trainIdx, 10)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(wine), trainIdx)
head(remainingRows, 10)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- wine[trainIdx, ]
validationSet <- wine[validationIdx, ]

# Here you combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
testSet <- wine[-c(trainIdx, validationIdx), ]

# Chk
nrow(trainSet) + nrow(validationSet) + nrow(testSet)
nrow(wine)

# Another example with variable treatment
rm(list=ls()[-grep('wine', ls())])

# Train 50%/Validation 40% /Variable Treatment 10%
trainPercentRows      <- round(nrow(wine) %*% .5)
validationPercentRows <- round(nrow(wine) %*% .4)

# Sample index for training
trainIdx <- sample(1:nrow(wine), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(wine), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- wine[trainIdx, ]
validationSet <- wine[validationIdx, ]

# Here you combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
prepData <- wine[-c(trainIdx, validationIdx), ]

plan <- designTreatmentsN(prepData, 
                          names(prepData)[1:13],
                          'Proline')

# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)

# Compare the data cleaning step
names(wine)
names(treatedTrain)

head(trainSet[,c(1,14)])
head(treatedTrain[,c(16:19)])

# End
