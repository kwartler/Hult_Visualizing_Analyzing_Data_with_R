# A supporting function to render all Rmd files in the folder instead of one by one.
#' @param fileFormat is a string from rmarkdown::render() but defaults to html_document
#' @param rmdPath is the path to the folder to look for files with pattern .Rmd within list.files()
#' @param savePath the path to save the files

makePages <- function(fileFormat = "html_document", rmdPath, savePath = getwd()){
  # Search for path files
  tmpRMD <- list.files(rmdPath, pattern = '.Rmd', full.names = T)
  # Warn if not found
  if(length(tmpRMD) == 0){
    stop('no Rmd files are found, make sure you created them and that the working directory is NOT /docs but the top level because the function is searching for /docs within the wd.')
  }
  # Construct
  for(i in 1:length(tmpRMD)){
    rmarkdown::render(tmpRMD[i], fileFormat,
                      output_dir = savePath)
    print(paste('starting on', tmpRMD[i], 'as a', fileFormat))
  }
}

makePages(rmdPath = '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/C_Report_Automation/scripts/D_multiPageMarkdown')
# End