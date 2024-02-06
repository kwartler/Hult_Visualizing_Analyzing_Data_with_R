library(vtreat)
df <- read.csv('~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/H_optional_portfolio_project/data_collection/33K_2024-01-27_diamonds.csv')
#df$diamondType <- NULL 

plan <- designTreatmentsN(df, 
                          varlist = names(df)[1:13],
                          outcomename = 'diamondPrice')
treatedDF <- prepare(plan, df)
treatedDF <- treatedDF[,-grep('_cat', names(treatedDF))]

### Define your interquartile range - remove 25% lowest values, remove 25% highest values, leaving the 50% in the middle
boxplot(treatedDF$diamondPrice)
outlierIndex <- boxplot.stats(treatedDF$diamondPrice)$out
treatedDF <- treatedDF[-outlierIndex,]

renderRegressionForm <- function(lm_obj, original_df, fileName = NULL) {
  
  # Add libraries
  library(htmltools)
  library(stringr)
  
  # Nest a custom function to infer variable types from the original data
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
    
    infered_df <- data.frame(x = names(infered_types), type = infered_types, stringsAsFactors = FALSE, row.names = NULL)
    return(infered_df)
  }
  
  # drop variables in the original not used in the linear model
  drops <- setdiff(names(original_df), names(coefficients(lm_obj)))
  df <- original_df[,!names(original_df) %in% drops]
  
  # infer variables, and remove any non Word variable names (numbers & (Intercept) )
  df      <- inferVariableTypes(df)
  if(names(coef(lm_obj))[1]=='(Intercept)'){
    df <- rbind(df, data.frame(x = '(Intercept)', type = 'intercept'))
  } else {
    df$beta <- coef(lm_obj)
  }
  df$beta <- coef(lm_obj)
  df$x    <- gsub("\\W", "", df$x)
  
  # Initialize intercept
  intercept <- 0
  
  # Get the row indices for each type of variable
  idx_intercept <- which(df$x == "Intercept")
  idx_boolean   <- which(df$type %in% c("Boolean", "dummy"))
  idx_numeric   <- which(df$type == "numeric")
  idx_character <- which(df$type == "character")
  
  # Process intercepts
  if (!is.null(idx_intercept)) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  # Process intercepts.
  if (length(idx_intercept) != 0) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  # Initialize the JavaScript function.
  jsfunc <- sprintf("function calculate() {\nlet result = %.2f;\n", intercept)
  
  # Process booleans and dummies.
  for (i in idx_boolean) {
    jsfunc <- paste0(jsfunc, sprintf("if (document.getElementById('%s').checked) { result += %.2f; }\n", df$x[i], df$beta[i]))
  }
  
  # Process numerics.
  for (i in idx_numeric) {
    jsfunc <- paste0(jsfunc, sprintf("result += document.getElementById('%s').value * %.2f;\n", df$x[i], df$beta[i]))
  }
  
  # Warning for character vectors.
  if (length(idx_character) > 0) {
    warning(sprintf("Ignoring character inputs for: %s", paste(df$x[idx_character], collapse = ", ")))
  }
  
  # Finish Javascript function
  jsfunc <- paste0(jsfunc, "document.getElementById('result').innerHTML = 'Result: ' + result;\n", "}")
  
  # Start HTML body with Intercept and Form
  htmlBody <- tags$body(
    tags$div(id = "intercept", tags$p(sprintf("Intercept: %.2f", intercept))),
    tags$form(id = "myForm")
  )
  
  # Add input mechanism according to type
  for(i in 1:nrow(df)) {
    if(df$type[i] == "Boolean" | df$type[i] == "dummy") {
      htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                        tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i])))
    } else if(df$type[i] == "numeric") {
      htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                        tags$div(tags$label(df$x[i]), tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0")))
    }
  }
  
  # Add Submit button
  htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, tags$input(type = "button", value = "Submit", onclick = "calculate()"))
  
  # Add result div and script
  htmlBody <- tagAppendChild(htmlBody, tags$div(id = "result"))
  htmlBody <- tagAppendChild(htmlBody, tags$script(HTML(jsfunc, "\n")))
  
  if(is.null(fileName)){
    warning('no fileName added, using default')
    fileName <- paste0(Sys.Date(), '_protypeRegForm.html')
  }
  save_html(htmlBody, fileName)
}


fit <- lm(diamondPrice ~., treatedDF)
bestFit <- step(fit, direction = 'backward')
summary(bestFit)

formDF <- data.frame(x = names(bestFit$coefficients),
                     beta = bestFit$coefficients,
                     row.names = NULL)


renderRegressionForm(lm_obj = bestFit, 
                     original_df = treatedDF, fileName = '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles/test.html')
