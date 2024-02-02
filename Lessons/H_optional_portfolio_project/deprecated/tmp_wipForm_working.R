renderRegressionForm <- function(df, fileName = NULL) {
  # Add libraries
  library(htmltools)
  library(stringr)
  
  df$x <- gsub("\\W", "", df$x) # Remove non-alphanumeric symbols to create valid JavaScript variable names
  
  # Initialize intercept
  intercept <- 0
  
  # Get the indices of df's rows for each type of input
  idx_intercept <- which(df$type == "intercept")
  idx_boolean <- which(df$type %in% c("Boolean", "dummy"))
  idx_numeric <- which(df$type == "numeric")
  idx_character <- which(df$type == "character")
  
  # Process intercepts
  if (length(idx_intercept) != 0) {
    intercept <- sum(df$beta[idx_intercept])
  }
  
  # Initialize the JavaScript function
  jsfunc <- sprintf("function calculate() {\nlet result = %.2f;\n", intercept)
  
  # Processes booleans and dummies
  for (i in idx_boolean) {
    jsfunc <- paste0(jsfunc, sprintf("if (document.getElementById('%s').checked) { result += %.2f; }\n", df$x[i], df$beta[i]))
  }
  
  # Process numerics
  for (i in idx_numeric) {
    jsfunc <- paste0(jsfunc, sprintf("result += document.getElementById('%s').value * %.2f;\n", df$x[i], df$beta[i]))
  }
  
  # Warning for character vectors
  if (length(idx_character) > 0) {
    warning(sprintf("Ignoring character inputs for: %s", paste(df$x[idx_character], collapse = ", ")))
  }
  
  # Finish Javascript function
  jsfunc <- paste0(jsfunc, "document.getElementById('result').innerHTML = 'Result: ' + result;\n", "}")
  
  # Starting CSS script
  css <- '
  body {
    background-color: #f0f0f7;
    font-family: Arial, sans-serif;
  }
  .container {
    width: 60%;
    margin: auto;
    background-color: #fff;
    padding: 20px;
    border-radius: 15px;
    box-shadow: 0 0 10px 0 #bbb;
  }
  label {
    display: inline-block;
    margin-top: 14px;
    font-weight: bold;
  }
  input[type="number"],
  input[type="submit"] {
    display: block;
    width: 100%;
    padding: 10px 15px;
    margin-top: 5px;
    border-radius: 5px;
    border: 1px solid #ccc;
  }
  input[type="number"]:focus {
    border-color: #77aacc;
  }
  input[type="submit"] {
    background-color: #77aacc;
    color: white;
    border-color: #77aacc;
    cursor: pointer;
  }
  input[type="submit"]:hover {
    background-color: #56a4d8;
  }
  #result {
    margin-top: 20px;
    background-color: #eaffff;
    padding: 20px;
    border-radius: 5px;
  }
  '
  # Start HTML body with a Bootstrap container, Intercept, Form, and added CSS
  htmlBody <- tags$body(
    tags$style(HTML(css)),
    tags$div(class = "container",
             tags$div(id = "intercept", tags$p(sprintf("Intercept: %.2f", intercept))),
             tags$form(id = "myForm")
    )
  )
  
  # Add input mechanism according to type
  for(i in 1:nrow(df)) {
    if(df$type[i] == "Boolean" || df$type[i] == "dummy") {
      htmlBody$children$htmlBody$container$form <- tagAppendChild(
        htmlBody$children$htmlBody$container$form, 
        tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i]))
      )
    } else if(df$type[i] == "numeric") {
      new_div <- tags$div(
        tags$label(paste0(df$x[i], " (enter numerical value)")),
        tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0")
      )
      htmlBody$children$htmlBody$container$form <- tagAppendChild(htmlBody$children$htmlBody$container$form, new_div)
    }
  }
  
  
  # Add Submit button
  htmlBody$children$htmlBody$container$form <- tagAppendChild(htmlBody$children$htmlBody$container$form, tags$input(type = "submit", value = "Submit", onclick = "calculate()"))
  
  # Add result div
  htmlBody$children$htmlBody$container <- tagAppendChild(htmlBody$children$htmlBody$container, tags$div(id = "result"))
  
  # Add JavaScript script
  htmlBody <- tagAppendChild(htmlBody, tags$script(HTML(jsfunc, "\n")))
  
  # Save to file
  if(is.null(fileName)){
    warning('no fileName added, using default')
    fileName <- paste0(Sys.Date(), '_protypeRegForm.html')
  }
  save_html(htmlBody, fileName)
}
renderRegressionForm(df, fileName = '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles/example10.html')
