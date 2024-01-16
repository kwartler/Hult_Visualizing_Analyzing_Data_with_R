scrapeSingleDiamond <- function(diamondirectURL){
  onePg <- URLencode(diamondirectURL)
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
  diamondDimensions <- lapply(strsplit(diamondDimensions, 'x'), as.numeric)
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
                         diamondX, diamondY, diamondZ, diamondCertification)
  
  return(response)
}