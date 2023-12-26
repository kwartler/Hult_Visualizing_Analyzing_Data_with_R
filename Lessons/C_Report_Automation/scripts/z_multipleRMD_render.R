# A supporting function to render all Rmd files in the folder instead of one by one.
#' @param fileFormat is a string from rmarkdown::render() but defaults to html_document
#' @param rmdPath is the path to the folder to look for files with pattern .Rmd within list.files()

makePages <- function(fileFormat = "html_document", rmdPath){
  # Search for path files
  tmpRMD <- list.files(rmdPath, pattern = '.Rmd', full.names = T)
  # Warn if not found
  if(length(tmpRMD) == 0){
    stop('no Rmd files are found, make sure you created them and that the working directory is NOT /docs but the top level because the function is searching for /docs within the wd.')
  }
  # Construct
  for(i in 1:length(tmpRMD)){
    rmarkdown::render(tmpRMD[i], fileFormat)
    print(paste('starting on', tmpRMD[i], 'as a', fileFormat))
  }
}

makePages(rmdPath = '~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/DD1/C_Mar6/scripts/D_multiPageMarkdown')
# End