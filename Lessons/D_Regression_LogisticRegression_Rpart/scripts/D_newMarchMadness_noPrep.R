#' Author: Ted Kwartler
#' Date: Feb 26, 2023
#' Purpose: Fit a robust logistic regression on basketball data w/o data prep shown
#' 

# Data Source (check kaggle for updates!):
#https://www.kaggle.com/competitions/march-machine-learning-mania-2023/data?select=MRegularSeasonDetailedResults.csv

# Caveats:
# There are many ways to structure the data.  This method assumes no interaction between teams ie rivalies and assumes the seasonal stat averages are indicative of the round one tournament outcome, it neglects teams that play "up" or "down" at tourney time. This structure takes away "target leakage" because all seasonal stats are complete before the tournament, so you would know it at inference (before the tourney).  It does *not* take into account averages that are skewed from a conference championship, where some teams play conference games and some do not.   


# You can always add more x-vars, engineer vars  like location, conference, "top 25 team" flag, player heights etc but let's keep it relatively simple.
# FGM - field goals made
# FGA - field goals attempted
# FGM3 - three pointers made
# FGA3 - three pointers attempted
# FTM - free throws made
# FTA - free throws attempted
# OR - offensive rebounds
# DR - defensive rebounds
# Ast - assists
# TO - turnovers
# Stl - steals
# Blk - blocks
# PF - personal fouls

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)
library(ggplot2)
library(dplyr)

# Data IO
modelingDF <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/modelingDF.csv')

# Seeds aren't really numbers so let's change that
modelingDF$Seed <- factor(modelingDF$Seed, order = T, levels = 16:1)
head(modelingDF$Seed,32)

########## EXPLORE & CLEAN
# See other script

# MODIFY
# Identify the informative and target
names(modelingDF)
targetVar       <- names(modelingDF)[4]
informativeVars <- names(modelingDF)[c(5, 7:20)] # model without seed: names(modelingDF)[c(7:20)]

#### SAMPLE
# Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(modelingDF),.1*nrow(modelingDF))
prepData    <- modelingDF[idx,]
nonPrepData <- modelingDF[-idx,]
nameKeeps <- modelingDF$Name[-idx] #tracking team name for later

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
treatedX <- prepare(plan, nonPrepData)

#### MODIFY Further
# Partition to avoid over fitting
set.seed(2022)
idx        <- sample(1:nrow(treatedX),.8*nrow(treatedX))
train      <- treatedX[idx,]
validation <- treatedX[-idx,]

#### MODEL
# Fit a logistic regression model
fit <- glm(TournamentWin ~., data = train, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
bestFit <- step(fit, direction='backward')
#saveRDS(bestFit, 'bestFit.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
teamPreds <- predict(bestFit,  validation, type='response')
tail(teamPreds)

# Classify 
cutoff      <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

#### ASSESS
# Organize w/Actual
results <- data.frame(actual  = nonPrepData[-idx,]$TournamentWin,
                      team    = nonPrepData[-idx,]$TeamName,
                      seed    = nonPrepData[-idx,]$Seed,
                      classes = teamClasses,
                      probs   = teamPreds)
head(results)

#### GO TO PPTX for explanation

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$classes, results$actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)
Accuracy(results$classes, results$actual)

# Visually how well did we separate our classes?
ggplot(results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'darkgreen')

# ROC; chg boolean to 1/0
ROCobj <- roc(results$classes, results$actual*1)
plot(ROCobj)

# AUC; chg boolean to 1/0
AUC(results$actual*1,results$classes)

# Increase the cutoff to improve balanced accuracy
# You can do this programmatically following this blog: https://rpubs.com/raysunau/optimal_cutoff_point
newCutoff <- .55
newClasses <- ifelse(teamPreds >= newCutoff, 1,0)
(confMat <- ConfusionMatrix(newClasses, results$actual))
Accuracy(newClasses, results$actual)

# So how do you score a new team? 
# Scrape, or manually put the data in the form the prepare function expects from the original modelinDF
# https://www.sports-reference.com/cbb/schools/notre-dame/men/2022.html
# Winning Pct https://www.sports-reference.com/cbb/schools/notre-dame/men/
oneTeamExample <- data.frame(Season = 2023, Seed = factor(12, order = T, levels = 16:1), 
                             TeamName = 'Notre Dame', FGM =26.1, FGA=56.4, FGM3 =9.1,
                             FGA3=23.8, FTM=11.3, FTA=15, OR=7.2, DR=26.8, Ast=14, TO=10.4, Stl=5.2, Blk = 1.9, 
                             PF = 12.7, RegularSeasonWin= .344)
oneTeamExample <- prepare(plan, oneTeamExample)

predict(bestFit,  oneTeamExample, type='response')

multipleTeams <- data.frame(Season = c(2023,2023),
                            Seed = factor(c(1,8), order = T, levels = 16:1),
                            TeamName = c('Alabama','TexasAM'),
                            FGM =c(32.6,23.9), 
                            FGA=c(61.7,54.8), 
                            FGM3 =c(7.8,6.2),
                            FGA3=c(20,19), 
                            FTM=c(14.5,19.4), 
                            FTA=c(20.8,25.4), 
                            OR=c(10.2,12.1), 
                            DR=c(26.2,24.7), 
                            Ast=c(16.6,12.8), 
                            TO=c(10.6,12.3), 
                            Stl=c(7.6,6.9), 
                            Blk = c(3.3,2.4), 
                            PF = c(16.8,18.4), 
                            RegularSeasonWin= c(.875,.741))
multipleTeams <- prepare(plan, multipleTeams)

predict(bestFit,  multipleTeams, type='response')


# End