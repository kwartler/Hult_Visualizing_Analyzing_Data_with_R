#' Author: Ted Kwartler
#' Date: Jan 23 2024
#' Purpose: Show how to execute the Rmd file to HTML knit into another directory

# Libs
library(rmarkdown)

# Where is the file located?
rmdFile <- '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/C_Report_Automation/scripts/C_flexdashboard.html'

# Where do you want to save the file?
saveLocation <- getwd()

# What do you want to call the html file?
fileName <- paste0('diabetes_weekly_dashboard_', Sys.Date())

# Render and save in the new location
rmarkdown::render(
  input         = rmdFile,
  output_format = "html_document",
  output_dir    = saveLocation,
  output_file   = fileName)  # specify a different output file name
# End