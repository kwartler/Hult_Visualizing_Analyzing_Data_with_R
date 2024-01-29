library(vtreat)
source("~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/H_optional_portfolio_project/beta_renderRegressionForm.R")
diamondData <- read.csv('33K_2024-01-27_diamonds.csv')
plan <- designTreatmentsN(diamondData,
                          names(diamondData)[1:14],
                          'diamondPrice')

train <- prepare(plan, diamondData)
drops <- names(train)[grep('_cat', names(train))]
train <- train[,!(names(train) %in% drops)]
fit <- lm(diamondPrice~., train)
bestFit <- step(fit, direction='backward')


###
inferVariableTypes <- function(original_df) {
  infered_types <- sapply(original_df, function(col) {
    unique_values <- unique(col)
    
    if (is.numeric(col)) {
      if(all(unique_values %in% c(0, 1)) & all(floor(unique_values) == unique_values)) {
        "dummy"
      } else {
        "numeric"
      }
    } else if (is.logical(col)) {
      "Boolean"
    } else if (is.character(col)) {
      "character"
    }
  })
  
  infered_df <- data.frame(x = names(infered_types), type = infered_types, stringsAsFactors = FALSE)
  return(infered_df)
}

varTypes <- inferVariableTypes(train)


##
df <- data.frame(x = names(bestFit$coefficients),
                 beta = bestFit$coefficients)
df <- left_join(df, varTypes, by = c('x'='x'))
df$type[1] <- 'intercept'


renderRegressionForm(df, '~/Desktop/Harvard_DataMining_Business_Student/personalFiles/example.html')



