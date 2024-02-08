renderRegressionForm_CSS <- function(lm_obj, original_df, fileName = NULL) {
  
  # Add libraries
  library(htmltools)
  library(stringr)
  
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
  
  drops <- setdiff(names(original_df), names(coefficients(lm_obj)))
  df <- original_df[,!names(original_df) %in% drops]
  
  df <- inferVariableTypes(df)
  if(names(coef(lm_obj))[1]=='(Intercept)'){
    df <- rbind(data.frame(x = '(Intercept)', type = 'intercept'), df)
  } else {
    df$beta <- coef(lm_obj)
  }
  df$beta <- coef(lm_obj)
  df$x <- gsub("\\W", "", df$x)
  
  intercept <- 0
  
  idx_intercept <- which(df$x == "Intercept")
  idx_boolean   <- which(df$type %in% c("Boolean", "dummy"))
  idx_numeric   <- which(df$type == "numeric")
  idx_character <- which(df$type == "character")
  
  if (!is.null(idx_intercept)) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  if (length(idx_intercept) != 0) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  jsfunc <- sprintf("function calculate() {\nlet result = %.2f;\n", intercept)
  
  for (i in idx_boolean) {
    jsfunc <- paste0(jsfunc, sprintf("if (document.getElementById('%s').checked) { result += %.2f; }\n", df$x[i], df$beta[i]))
  }
  
  for (i in idx_numeric) {
    jsfunc <- paste0(jsfunc, sprintf("result += document.getElementById('%s').value * %.2f;\n", df$x[i], df$beta[i]))
  }
  
  if (length(idx_character) > 0) {
    warning(sprintf("Ignoring character inputs for: %s", paste(df$x[idx_character], collapse = ", ")))
  }
  
  jsfunc <- paste0(jsfunc, "document.getElementById('result').innerHTML = 'Result: ' + result;\n", "}")
  
  # Create form tag without attaching it to the body yet
  myForm <- tags$form(id = "myForm")
  
  # Add input mechanism according to type
  for(i in 1:nrow(df)) {
    if(df$type[i] == "Boolean" | df$type[i] == "dummy") {
      myForm <- tagAppendChild(myForm, 
                               tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i])))
    } else if(df$type[i] == "numeric") {
      myForm <- tagAppendChild(myForm, 
                               tags$div(tags$label(df$x[i]), tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0")))
    }
  }
  
  # Add Submit button
  myForm <- tagAppendChild(myForm, tags$input(type = "button", value = "Submit", onclick = "calculate()"))
  
  myForm <- tagAppendChild(myForm, tags$style(HTML("
                                          input[type='number'] {
                                            width: 100%;
                                            padding: 12px 20px;
                                            margin: 8px 0;
                                            box-sizing: border-box;
                                            border: 2px solid #555;
                                            border-radius: 4px;
                                          }
                                          
                                          input[type='button'] {
                                            background-color: #4CAF50;
                                            border: none;
                                            color: white;
                                            padding: 16px 32px;
                                            text-decoration: none;
                                            margin: 4px 2px;
                                            cursor: pointer;
                                          }
                                          ")))
  
  # Now that `myForm` is created we can reference it in `htmlBody`
  #box-shadow: 0 0 20px 0 rgba(0, 0, 0, 0.1);
  htmlBody <- tags$body(
    style = "background-color: #A100FF",
    tags$div(
      style = "max-width: 500px; overflow-y: auto; margin: auto; background-color: #f2f2f2; padding: 20px; border-radius: 5px; box-shadow: 5px 10px #888888; font-family: Arial;",
      tags$p(sprintf("Intercept: %.2f", intercept), style = "font-size: 18px; text-align:center"),
      myForm  
    )
  )
  
  # Add result div and script
  htmlBody <- tagAppendChild(htmlBody, tags$div(id = "result", style = "font-family: Arial; font-size: 24px; text-align: center; color: white")) 
  
  #htmlBody <- tagAppendChild(htmlBody, tags$div(id = "result", style = "font-family: Arial; font-size: 18px; text-align: center")) 
  htmlBody <- tagAppendChild(htmlBody, tags$script(HTML(jsfunc, "\n")))
  
  if(is.null(fileName)){
    warning('no fileName added, using default')
    fileName <- paste0(Sys.Date(), '_protypeRegForm.html')
  }
  save_html(htmlBody, fileName)
}
