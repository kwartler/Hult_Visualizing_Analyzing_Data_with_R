library(htmltools)
library(stringr)

renderRegressionForm <- function(df, fileName = NULL){
  
  df$x <- gsub("\\W", "", df$x) 
  
  intercept <- 0
  idx_intercept <- which(df$type == "intercept")
  idx_boolean <- which(df$type %in% c("Boolean", "dummy"))
  idx_numeric <- which(df$type == "numeric")
  idx_character <- which(df$type == "character")
  
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
  
  cssCode <- "
body {
    font-family: Arial, sans-serif;
}

form {
    width: 300px;
    margin: 0 auto;
}

form div {
    margin-bottom: 15px;
}

form input[type='number'], 
form input[type='checkbox'] {
    width: 100%;
    padding: 7px;
    border-radius: 5px;
    border: 1px solid grey;
}

form input[type='button'] {
    background-color: #4CAF50;
    border: none;
    color: white;
    padding: 15px 32px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
    margin: 4px 2px;
    cursor: pointer;
    border-radius: 5px;
    width: 100%;
}
  " 

htmlBody <- tags$body(
  tags$style(HTML(cssCode)),
  tags$div(id = "intercept", tags$p(sprintf("Intercept: %.2f", intercept))),
  tags$form(id = "myForm"))

for(i in 1:nrow(df)) {
  if(df$type[i] == "Boolean" | df$type[i] == "dummy") {
    htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                      tags$div(tags$label(df$x[i]), tags$input(type = "checkbox", id = df$x[i], name = df$x[i])))
  } else if(df$type[i] == "numeric") {
    htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, 
                                                      tags$div(tags$label(df$x[i]), tags$input(type = "number", step = "0.01", id = df$x[i], name = df$x[i], value = "0")))
  }
}

htmlBody$children$htmlBody$form <- tagAppendChild(htmlBody$children$htmlBody$form, tags$input(type = "button", value = "Submit", onclick = "calculate()"))

htmlBody <- tagAppendChild(htmlBody, tags$div(id = "result"))
htmlBody <- tagAppendChild(htmlBody, tags$script(HTML(jsfunc, "\n")))

if(is.null(fileName)){
  warning('no fileName added, using default')
  fileName <- paste0(Sys.Date(), '_protypeRegForm.html')
}
save_html(htmlBody, fileName)
}
renderRegressionForm(df, '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles/example10.html')