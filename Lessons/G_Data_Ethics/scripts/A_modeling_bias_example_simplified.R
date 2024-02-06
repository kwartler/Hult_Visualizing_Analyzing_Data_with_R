# Set WD
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")
options(scipen = 999)


# Libs
library(caret)
library(tm)
library(glmnet)
library(MLmetrics)
library(vtreat)
library(fairness)

# Custom cleaning function
resumeClean<-function(xVec, stops=stopwords("SMART")){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  xVec <- removeWords(xVec, stops)
  return(xVec)
}

# Data
candidates <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/H_Mar21/data/HR%20Hiring%20(Bias%20%26%20Fairness).csv')

### SAMPLE : Partitioning
set.seed(1234)
idx             <- createDataPartition(candidates$Hired,p=.7,list=F)
trainCandidates <- candidates[idx,]
testCandidates  <- candidates[-idx,]

### EXPLORE
head(as.data.frame(trainCandidates),2)

table(trainCandidates$Hired)

### MODIFY
trainCandidates$Summary <- resumeClean(as.character(trainCandidates$Summary))

# Tokenize and Make vocab
trigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse = " "), 
         use.names = FALSE)
}

txtCorpus <- VCorpus(VectorSource(trainCandidates$Summary))
resumeTDM  <- DocumentTermMatrix(txtCorpus, 
                                control=list(tokenize=trigramTokens))
resumeTDMm <- as.matrix(resumeTDM)

dim(resumeTDMm)

# Append DTM to original data
allCandidateData <- cbind(trainCandidates, resumeTDMm)
names(allCandidateData)[c(179,169,147)]

# Let's drop ApplicationID,AgeBracket, Gender and raw text Summary
drops <- c('ApplicationID', 'AgeBracket', 'Gender', 'Summary')
allCandidateData <- allCandidateData[, !(names(allCandidateData) %in% drops)]

# Now let's prepare for modeling by making dummy variables
#plan <- designTreatmentsC(allCandidateData, #data
#                          names(allCandidateData), #x-var columns
#                          'Hired', # y-var name
#                          'Yes') #success factor level
#saveRDS(plan, 'variable_treatment_plan2.rds')

#update for your path if you don't build it above!
pth <- '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/G_Data_Ethics/data/variable_treatment_plan2.rds'
plan <- readRDS(pth)
allCandidateData <- prepare(plan, allCandidateData)

# Separate the y var & some useless engineered variables
allCandidateData <- allCandidateData[,-grep('Hired|_catP|_catB', 
                                            names(allCandidateData))]

### MODEL
candidateFit <- cv.glmnet(as.matrix(allCandidateData),
                          y            = as.factor(trainCandidates$Hired),
                          alpha        = 0.9,
                          family       = 'binomial',
                          type.measure = 'auc',
                          nfolds       = 3,
                          intercept    = F)
#saveRDS(candidateFit, 'candidateFit2.rds')

### ASSESS
# Get Predictions on the training set from all 3 models
trainPreds   <- predict(candidateFit,
                        as.matrix(allCandidateData),
                        type = 'class',
                        s    = candidateFit$lambda.min)
table(trainPreds, trainCandidates$Hired)
Accuracy(trainPreds, trainCandidates$Hired)

# Let's investigate further
trainDF <- data.frame(Preds  = trainPreds[,1], 
                      actuals    = trainCandidates$Hired,
                      AgeBracket = trainCandidates$AgeBracket,
                      Gender     = trainCandidates$Gender)
head(trainDF)

# Model behavior
# Test for equal representation "positive class parity" for every one
# 40 and over candidate predicted to be hired, 
# how many under 40 candidates are predicted to be hired? 
# Positive class for >40 / positive class for <40
dem_parity(data = trainDF, 
           outcome = 'actuals', 
           group = 'AgeBracket',
           preds = 'Preds', base = '40 and Over')

# What about gender?  For every male predicted to be hired, 
#how many females are predicted to be hired? 
dem_parity(data = trainDF, 
           outcome = 'actuals', 
           group = 'Gender',
           preds = 'Preds', base = 'Male')

# Since gender was removed, let's figure out whats happening.
genderFit <- cv.glmnet(as.matrix(allCandidateData),
                       y            = as.factor(trainCandidates$Gender), #predicting "male"
                       alpha        = 0.9,
                       family       = 'binomial',
                       type.measure = 'auc',
                       nfolds       = 3,
                       intercept    = F)

# Subset to impacting terms to identify issues for rebuilding the model
bestTerms <- subset(as.matrix(coefficients(genderFit)), 
                    as.matrix(coefficients(genderFit)) !=0)
bestTerms <- data.frame(term = rownames(bestTerms), value = bestTerms[,1])
bestTerms <- bestTerms[order(bestTerms$value, decreasing=T), ] #proxies

# Indicative of "male"
head(bestTerms, 15)
# Indicative of "female"
tail(bestTerms, 15)

# What about for age?
ageFit <- cv.glmnet(as.matrix(allCandidateData),
                    y=as.factor(trainCandidates$AgeBracket), #predicting "40 and Over"
                    alpha=0.9,
                    family='binomial',
                    type.measure='auc',
                    nfolds=3,
                    intercept=F)

# Subset to impacting terms to identify issues for rebuilding the model
bestTerms <- subset(as.matrix(coefficients(ageFit)), 
                    as.matrix(coefficients(ageFit)) !=0)
bestTerms <- data.frame(term = rownames(bestTerms), value = bestTerms[,1])
bestTerms <- bestTerms[order(bestTerms$value, decreasing=T), ] #proxies

# Indicative of "40 and Over"
head(bestTerms, 8)
# indicative of "under 40"
tail(bestTerms, 8)

# End
