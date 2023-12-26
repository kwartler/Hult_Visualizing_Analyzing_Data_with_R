#' Author: Ted Kwartler
#' Date: Feb 26, 2023
#' Purpose: Fit a robust logistic regression on basketball data w/data prep shown
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

#### Raw Data I/O
# Historical regular season box scores
bbStats <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/MRegularSeasonDetailedResults.csv')

# Historical tourney data.  You can't use these box stat scores because you don't know these stats when you need to make a prediction!  The stats happen during and after the game that you're predicting.  But we need to obtain whether the team won or lost from this data.
tourneyDetailedResults <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/MNCAATourneyDetailedResults.csv")

# Now let's get the seed stats, this is something you would know ahead of the tourney so its valid.
tourneySeeds <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/MNCAATourneySeeds.csv")

# Cross Walk with teamID to names
xWalk <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/MTeams.csv')


#### Data Prep
# The stats in our model
statsOfInterest <- c("FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF")

# Separate winning and losing team box scores
winners <- bbStats[ ,c('Season','WTeamID',paste0('W',statsOfInterest))]
losers  <- bbStats[ ,c('Season','LTeamID',paste0('L',statsOfInterest))]

# Align column names
colnames(winners) <- c('Season','TeamID', statsOfInterest)
colnames(losers)  <- c('Season','TeamID', statsOfInterest)

# Append winning or losing to get seasonal win pct
winners$RegularSeasonWin <- T
losers$RegularSeasonWin <- F

# Combine
allTeams <- rbind(winners, losers)

# Now get averages 
seasonalAvg <- allTeams %>%
  group_by(Season, TeamID) %>% 
  summarise(across(c(statsOfInterest,'RegularSeasonWin'), mean))

# Base R aggregate but needs more lines to accomplish the "across()" aspect
#seaonsalAvgWin <- aggregate(RegularSeasonWin ~Season + TeamID, allTeams, mean)

########## EXPLORE & CLEAN; in reality you would do a lot more viz and EDA to get to know the data
head(seasonalAvg)

# Now let's do something similar for the tourney results
# Separate but add a few more columns needed later
winners <- tourneyDetailedResults[ ,c('Season','WTeamID', 'WScore', 'DayNum', paste0('W',statsOfInterest))]
losers  <- tourneyDetailedResults[ ,c('Season','LTeamID', 'LScore', 'DayNum', paste0('L',statsOfInterest))]

# Align column names
colnames(winners) <- c('Season','TeamID', 'Score', 'DayNum', statsOfInterest)
colnames(losers)  <- c('Season','TeamID', 'Score', 'DayNum', statsOfInterest)

# Append winning or losing which is our logistic y-var
winners$TournamentWin <- T
losers$TournamentWin <- F

# Combine
allTourneyTeams <- rbind(winners, losers)

# Let's append the Seeds
allTourneyTeams <- left_join(allTourneyTeams, tourneySeeds, by = c('Season'='Season','TeamID'='TeamID'))
table(allTourneyTeams$Seed)
## Data Dictionary:
# W, X, Y, or Z (identifying the region the team was in) 
# two digits (see in the regoin)
# a or b (play in teams were added and expanded over time)

# Only focusing on R1
table(allTourneyTeams$DayNum, allTourneyTeams$Season)
# For most seasons this is the schema [not 2021!]
# DayNum 134 & 135 are play in days ie extra teams not part of the 64
# R1 (round of 64) = DayNum 136 & 137 
# R2 (round of 32) = DayNum 138 & 139 
# R3 (Sweet 16) = DayNum 143 & 144 
# R4 (Elite 8) = DayNum 145 & 146 
# R5 (Final 4) = DayNum 152 
# R6 (Championship) = DayNum 154 

# not2021 
not2021 <- subset(allTourneyTeams, allTourneyTeams$Season != 2021)
not2021 <- subset(not2021, not2021$DayNum==136 | not2021$DayNum==137)

# forever2021 - missing an odd game [2 rows]?!
forever2021 <- subset(allTourneyTeams, allTourneyTeams$Season == 2021)
forever2021 <- subset(forever2021, forever2021$DayNum==137 | forever2021$DayNum==138)

# Let's put them back together
allTourneyTeams <- rbind(not2021, forever2021)

# Let's assume the region, and play in games don't matter for seeds and just get them to be 1-16.  
# One could engineer a region column from $Seed substring(allTourneyTeams$Seed, 1, 1), but that's up to you 
# Keep in mind these are ordinal classes, not true numbers  
allTourneyTeams$Seed <- factor(as.numeric(gsub('[[:alpha:]]','',allTourneyTeams$Seed)), order = T, levels = 16:1)

# Examine and ordered factor class
head(allTourneyTeams$Seed, 32)
summary(allTourneyTeams$Seed)
class(allTourneyTeams$Seed)

# Let's build our final modeling matrix
modelingDF <- left_join(allTourneyTeams, xWalk[,1:2], by = 'TeamID')
keeps <- c('TeamName', 'TeamID','Season','Seed','Score', 'TournamentWin')
modelingDF <- modelingDF[names(modelingDF) %in% keeps]

# Append the end of season stats to the tourney data
modelingDF <- left_join(modelingDF, seasonalAvg, by = c('Season'='Season','TeamID'='TeamID'))

# Randomize the entire data set to ensure no auto-correlation 
set.seed(2023)
modelingDF <- modelingDF[sample(1:nrow(modelingDF),nrow(modelingDF)),]

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

#### MODIFY Further
# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
treatedX <- prepare(plan, nonPrepData)

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
oneTeamExample <- data.frame(Season = 2023, 
                             Seed = factor(12, order = T, levels = 16:1), 
                             TeamName = 'Notre Dame', 
                             FGM =26.1, 
                             FGA=56.4, 
                             FGM3 =9.1,
                             FGA3=23.8,
                             FTM=11.3, 
                             FTA=15, 
                             OR=7.2, 
                             DR=26.8, 
                             Ast=14, 
                             TO=10.4, 
                             Stl=5.2, 
                             Blk = 1.9, 
                             PF = 12.7, 
                             RegularSeasonWin= .344)
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

