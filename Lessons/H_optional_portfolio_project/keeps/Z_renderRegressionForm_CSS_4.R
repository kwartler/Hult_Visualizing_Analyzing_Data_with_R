#' @param lm_obj the object from the lm() function call
#' @param original_df the original data frame that was used in the lm build.  In this use case, its "treated" by vtreat to ensure dummy variables and factors are accounted for.  Technically this is within the lm_obj list but we pass it in separately for learning easier understanding
#' @param fileName the file path and name, something like 'my_model.html'

renderRegressionFormCSS4 <- function(lm_obj, original_df, fileName = NULL) {
  
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
    df <- rbind(data.frame(x = '(Intercept)', type = 'intercept'), df)
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
  
  ### CSS Improvements
  # Start forming a list of form elements
  form_elements <- list(tags$div(id = "intercept", tags$p(sprintf("Intercept: %.2f", intercept))),
                        tags$form(id = "myForm"))
  form_elements <- c(form_elements, list(tags$div(id = "result", class = "result")))
  
  
  # Add form input elements
  for(i in 1:nrow(df)) {
    new_div <- NULL
    if(df$type[i] == "Boolean" | df$type[i] == "dummy") {
      new_div <- tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i]))
    }
    else if(df$type[i] == "numeric") {
      new_div <- tags$div(tags$label(df$x[i]), tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0"))
    }
    form_elements <- c(form_elements, list(new_div))
  }
  
  # Add Submit button
  form_elements <- c(form_elements, list(tags$input(type = "button", value = "Submit", onclick = "calculate()")))
  
  # Add result div
  form_elements <- c(form_elements, list(tags$div(id = "result")))
  
  # Finish javascript function
  jsfunc <- paste0(jsfunc, "document.getElementById('result').innerHTML = 'Result: ' + result;\n}", "\n")
  form_elements <- c(form_elements, list(tags$script(HTML(jsfunc))))
  
  # Start CSS
  css <- '
  body {
    font-family: Arial, sans-serif;
    background-color: #f4f4f4;
  }
  .form-container {
    max-width: 500px;
    margin: auto;
    background-color: white;
    padding: 2em;
    border-radius: 15px;
    box-shadow: 0 0 20px 0 rgba(0, 0, 0, 0.1);
  }
  form {
    margin: 1em 0;
  }
  label {
    display: block;
    margin: 0.5em 0;
  }
  input[type="number"], input[type="checkbox"] {
    margin: 0.5em;
  }
  input[type="button"] {
    background-color: #4CAF50; /* Green */
    border: none;
    color: white;
    padding: 15px 32px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
    margin: 4px 2px;
    transition-duration: 0.4s;
    cursor: pointer;
  }
  input[type="button"]:hover {
    background-color: #45a049;
  }
  #result {
    background-color: #f2f2f2;
    padding: 1em;
    margin: 1em 0;
  }
  '
  
  # Create container for the form and append elements
  formContainer <- tags$div(class = "form-container", form_elements)
  
  # Append the style block
  htmlBody <- tagAppendChild(formContainer, tags$style(HTML(css)))
  
  # Save modified html file
  if(is.null(fileName)){
    warning('no fileName added, using default')
    fileName <- paste0(Sys.Date(), '_protypeRegForm.html')
  }
  save_html(htmlBody, fileName)
  
}