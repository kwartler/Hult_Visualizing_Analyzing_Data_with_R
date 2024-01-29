#oneDiamond <- 'https://diamondsdirect.com/oval-0-5-f-vs2-307980/?c_id=149&m=category&c=Natural Diamonds'
#Look at all css classes on the page:
# onePg <- URLencode(oneDiamond)
# pg <- read_html(onePg)
#pg %>% 
#  html_nodes("*") %>% 
#  html_attr("class") %>% 
#  unique()
# Diamond attributes review
#pg %>% html_nodes('.product-view-section') %>% 
# html_nodes("*") %>% 
# html_attr("class") %>% 
# unique()

#' @param diamondirectURL string of the URL to be scraped
#' @param waitTime Boolean if a random fraction of a second should delay the html reading; default is TRUE.  This can aid with not being black listed on a site
scrapeSingleDiamond <- function(diamondirectURL, waitTime = T){
  onePg <- URLencode(diamondirectURL)
  if(waitTime==T){
    Sys.sleep(sample(seq(0,1, by =.05),1))
  }
  pg    <- read_html(onePg)
  
  # Check for price and if not found return NA
  diamondPrice <- pg %>% html_nodes('.price--withoutTax')  %>% html_text()
  diamondPrice <- gsub("\\$|,",'',diamondPrice)
  
  # Diamond type
  diamondType <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.type') %>% html_text() 
  diamondType <- trimws(gsub('Type:','',diamondType))
  
  # Diamond shape
  diamondShape <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.shape') %>% html_text() 
  diamondShape <- trimws(gsub('Shape:','',diamondShape))
  
  # Diamond carat
  diamondCarat <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.carat') %>% html_text() 
  diamondCarat <- trimws(gsub('Carat:','',diamondCarat))
  
  # Diamond color
  diamondColor <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.color') %>% html_text() 
  diamondColor <- trimws(gsub('Color:','',diamondColor))
  
  # Diamond clarity
  diamondClarity <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.clarity') %>% html_text() 
  diamondClarity <- trimws(gsub('Clarity:','',diamondClarity))
  
  # Diamond polish
  diamondPolish <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.polish') %>% html_text() 
  diamondPolish <- trimws(gsub('Polish:','',diamondPolish))

  # Diamond symmetry
  diamondSymmetry <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.symmetry') %>% html_text() 
  diamondSymmetry <- trimws(gsub('Symmetry:','',diamondSymmetry))

  # Diamond fluorescence
  diamondFluorescence <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.fluorescence') %>% html_text() 
  diamondFluorescence <- trimws(gsub('Fluorescence:','',diamondFluorescence))

  # Diamond table
  diamondTable <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.table') %>% html_text() 
  diamondTable <- trimws(gsub('Table:','',diamondTable))
  diamondTable <- as.numeric(gsub(' %','',diamondTable))/100
  
  # Diamond depth
  diamondDepth <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.depth') %>% html_text() 
  diamondDepth <- trimws(gsub('Depth:','',diamondDepth))
  diamondDepth <- as.numeric(gsub(' %','',diamondDepth))/100
  
  # Diamond dimensions-(mm)
  # Remember to escape the ( and ) with \\
  diamondDimensions <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.dimensions-\\(mm\\)') %>% html_text() 
  diamondDimensions <- trimws(gsub('Dimensions \\(mm\\):','',diamondDimensions))
  diamondDimensions <- lapply(strsplit(diamondDimensions, 'x|-|[*]'), as.numeric)
  diamondDimensions <- unlist(diamondDimensions)
  diamondX <- diamondDimensions[1]
  diamondY <- diamondDimensions[2]
  diamondZ <- diamondDimensions[3]
  
  # Diamond certification
  diamondCertification <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.certification') %>% html_text() 
  diamondCertification <- trimws(gsub('Certification:','',diamondCertification))
  
  # Organize 
  response <- data.frame(diamondType, diamondShape,diamondCarat,diamondColor,
                         diamondClarity, diamondPolish, diamondSymmetry,
                         diamondFluorescence, diamondTable, diamondDepth,
                         diamondX, diamondY, diamondZ, diamondCertification,
                         diamondPrice)
  
  return(response)
}


tryToScrapeDiamond <- function(diamondirectURL, waitTime=T) {
  # Start with NULL value
  response <- NULL
  
  # Try to get the info & capture the error if thrown
  tryError <- tryCatch({
    response <- scrapeSingleDiamond(diamondirectURL, waitTime)
  }, 
  error = function(e) {
    #print(e)
    e
  })
  
  # if there is an error, set response back to NULL
  if (inherits(tryError, 'error')) 
    response <- NULL
  
  return(response)
}

### How does tryCatch work?
#tryCatch({
#  # the expression to try
#},
#error = function(e) {
#  # what to do in case of error
#},
#warning = function(w) {
#  # what to do in case of warning
#},
#finally = {
#  # cleanup actions
#})
#### End