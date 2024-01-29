#' Title: Collect up to date diamonds data
#' Purpose: Build a regression model for use in a portfolio project
#' Author: TK
#' Date: Jan 10, 2024

library(rvest)
library(tidyverse)
library(pbapply)

# Load our custom function
source("~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/H_optional_portfolio_project/Z_scrapeSingleDiamond.R")

# Inputs
# Should you save the raw data
saveData <- T

# Change this to a personal file folder
savePth  <- '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/H_optional_portfolio_project/data_collection'

# https://diamondsdirect.com/diamonds/
maxPages <- 796

allURLs <- paste0('https://diamondsdirect.com/diamonds/?from=',1:maxPages) # Just ensure last page

# To avoid black listing breaking up into multiple loops 
# Gather each diamond's individual page
allURLS <- list()
for(i in 1:length(allURLs)){
  print(i)
  #waitTime <- sample(seq(0.05,1, by =0.05),1)
  #cat(paste('wait time: ',waitTime, 'working on number:',i,'of', length(allURLs),'\n'))
  #Sys.sleep(waitTime)
  pg <- read_html(allURLs[i])
  individualRockURL <- pg %>%
    html_nodes('.card-inner') %>% 
    html_nodes('a') %>% 
    html_attr('href')
  res <- paste0('https://diamondsdirect.com',individualRockURL)
  allURLS[[i]] <- res
}
x <- unlist(allURLS)
x <- x[!duplicated(x)]
if(saveData==T){
  pth <- file.path(savePth,paste0(Sys.Date(), '_allDiamond_URLS.txt'))
  writeLines(x, pth)
}

# Now read specific diamond info as an example
oneDiamond <- scrapeSingleDiamond(x[2])
tryToScrapeDiamond(x[2])
anotherDiamond <- scrapeSingleDiamond(x[10000], waitTime=F)

# Since we don't want this to fail we use the tryCatch version with lapply
# allDiamonds <- pblapply(x[10001:10010], tryToScrapeDiamond, waitTime=F)

#allDiamonds <- pblapply(x, tryToScrapeDiamond)
allDiamonds <- list()
for(i in 1:length(x)){ # start at 601
  print(i)
  tmpDiamond <- tryToScrapeDiamond(x[i], waitTime=F)
  allDiamonds[[i]] <- tmpDiamond
}

# In case there are NULL elements and the page couldn't be scraped lets drop them
allDiamondsDF <- allDiamonds[!sapply(allDiamonds, is.null)]

# Organize into a data frame
allDiamondsDF <- do.call(rbind, allDiamondsDF)

write.csv(allDiamondsDF, '33K_2024-01-27_diamonds.csv', row.names = F)




# End