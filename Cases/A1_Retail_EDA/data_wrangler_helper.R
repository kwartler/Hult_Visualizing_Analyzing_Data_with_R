#' Case A1 Data Helper
#' Purpose: To help students who may be struggling just with the data organization for case a1 
#' Ted Kwartler
#' Jan 21, 2024

# WD
setwd("~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles")

# Libraries
library(data.table)

# Name the case file you are creating
fileName <- 'a1_EDA_case.csv'

# Set this to your Cases/A1_Retail_EDA_data folder
caseData <- '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Cases/A1_Retail_EDA/data'

# Let's get all files from the folders
tmp <- list.files(path       = caseData,
                  pattern    = '*.csv',
                  full.names = T,
                  recursive  = T)

# Do you want to get all transactions or a sample?
sampleTransactions   <- F
nTransactionsPerFile <- 10000

caseData <- list()
for(i in 1:length(tmp)){
  print(paste('Reading in file',i, ':',tmp[i]))
  tmpTransactions <- fread(tmp[i])
  
  if(sampleTransactions==T){
    print(paste('Sampling data to',nTransactionsPerFile,'rows.'))
    tmpTransactions <- tmpTransactions[sample(1:nTransactionsPerFile),]
  } else {
    print('Ignoring nTransactionsPerFile & reading all data')
  }
  caseData[[i]] <- tmpTransactions
}

# Organize into a single data frame
caseData <- do.call(rbind, caseData)

# Save into a single file
if(sampleTransactions==T){
  nam <- paste0(Sys.Date(),'_sampled_', nrow(caseData),'_rows_',fileName)
} else {
    nam <- nam <- paste0(Sys.Date(),'_complete_data_', fileName)
  }

fwrite(caseData, nam)
# End