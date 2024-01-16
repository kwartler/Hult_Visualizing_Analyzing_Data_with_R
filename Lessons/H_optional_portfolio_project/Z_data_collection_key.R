#' Title: Collect up to date diamonds data
#' Purpose: Build a regression model for use in a portfolio project
#' Author: TK
#' Date: Jan 10, 2024

library(rvest)
library(tidyverse)

# Inputs
saveData <- T
savePth  <- '~/Desktop/Hult_Visualizing_Analyzing_Data_with_R/Lessons/H_optional_portfolio_project/data_collection'
maxPages <- 805

allURLs <- paste0('https://diamondsdirect.com/diamonds/?from=',1:maxPages) # Just ensure last page

# To avoid black listing breaking up into multiple loops 
# Gather each diamond's individual page
allURLS <- list()
for(i in 1:length(allURLs)){
  waitTime <- sample(seq(0.05,1, by =0.05),1)
  cat(paste('wait time: ',waitTime, 'working on number:',i,'of', length(allURLs),'\n'))
  Sys.sleep(waitTime)
  pg <- read_html(allURLs[i])
  individualRockURL <- pg %>%
    html_nodes('.card-inner') %>% 
    html_nodes('a') %>% 
    html_attr('href')
  res <- paste0('https://diamondsdirect.com',individualRockURL)
  allURLS[[i]] <- res
}
x <- unlist(allURLS)
x <- x[!duplicated(x)]
if(saveData==T){
  pth <- file.path(savePth,paste0(Sys.Date(), 'allDiamond_URLS.txt'))
  writeLines(x, pth)
}


# Now read specific diamond info
allDiamonds <- list()

for(i in 1:length(x)){
  onePg <- URLencode(x[i])
  pg <- read_html(onePg)
  
  #Look at all css classes on the page:
  #pg %>% 
  #  html_nodes("*") %>% 
  #  html_attr("class") %>% 
  #  unique()
  
  # Diamond attributes review
  #pg %>% html_nodes('.product-view-section') %>% 
  # html_nodes("*") %>% 
  # html_attr("class") %>% 
  # unique()
  
  
  
  
  
  # Diamond type
  diamondType <- pg %>% 
    html_nodes('.product-view-section') %>% 
    html_nodes('.type') %>% html_text() 
  diamondType <- trimws(gsub('Type:','',diamondType))
  
  # 
  
  pg %>% html_nodes('.product-image-section') %>%
    html_node('img') %>%
    html_attr('src')
  pg %>% html_nodes('.price--withoutTax')  %>% html_text()
  product-details-inner
}




for(i in 1:length(x)){ #start 18873:15027 is a problem; need to move the write.csv outside of else statement
  waitTime <- sample(seq(0,.35, by =0.05),1)
  cat(paste('wait time: ',waitTime, 'working on number:',i,'of', length(x),'\n'))
  #cat(paste('working on number:',i,'of', length(x),'\n'))
  #Sys.sleep(waitTime)
  onePg <- URLencode(x[i])
  pg <- read_html(onePg)

  chk <- is.na(pg %>%
                 html_node('.price--withoutTax') %>% html_text)
 if(chk==T|x[i]=='https://diamondsdirect.com'){
   stats <- data.frame(SKU='NA',
                       Shape='NA',
                       Carat='NA',
                       Clarity='NA',
                       Color='NA',
                       Cut='NA',
                       Polish='NA',
                       Symmetry='NA',
                       Lab='NA',
                       Fluorescence='NA',
                       Measurements='NA',
                       x='NA',
                       y='NA',
                       z='NA',
                       price='NA',
                       listingURL=x[i],
                       imgURL = 'NA')
   allDiamonds[[i]] <- stats 
 } else {
   price <- pg %>%
     html_node('.price--withoutTax') %>% html_text
   imgURL <- pg %>%
     html_nodes('.product-image-section') %>%
     html_node('img') %>%
     html_attr('src')
   stats <- pg %>% html_nodes('.product-info-list') %>% html_nodes('span')%>% html_text()
   stats <- stats[-1]
   statLabels <- pg %>% html_nodes('.product-info-list') %>% html_nodes('strong')%>% html_text()
   statLabels <- gsub(':','',statLabels)
   stats <- as.data.frame(t(stats))
   names(stats) <- make.names(statLabels)
   
   stats$SKU <- gsub('/', '',stats$SKU)
   stats$SKU <- paste0('sku_',stats$SKU)
   stats$Carat <- as.numeric(stats$Carat)
   #if(is.null(stats$Caret)){
   #  stats$Caret <- 'NA'
   #} else {
   #   stats$Carat <- as.numeric(stats$Carat)
   #}
   #if(is.na(price)){
   #  price <- 'NA'
   #} else {
   #  price  <- as.numeric(gsub('[$]|,','',price))
   #}
   dimCol <- grepl('Measurements|Dimensions', statLabels)
   tmp <- as.numeric(unlist(strsplit(stats[,dimCol],'x|-')))
   stats$x <- tmp[1]
   stats$y <- tmp[2]
   stats$z <- tmp[3]
   stats$Cut <- ifelse(is.null(stats$Cut), 'NA',stats$Cut)
   stats$price <- price
   stats$listingURL <-x[i] 
   stats$imgURL <- imgURL
   allDiamonds[[i]] <- stats 
 }
  
}

if(saveData==T){
  allDiamondsDF <- do.call(rbind, allDiamonds)
  pth <- file.path(savePth, Sys.Date(),'allDiamonds.csv')
  write.csv(allDiamondsDF, pth, row.names = F)
}
x <- list.files(path = '~/Desktop/DataMiningCases/diamondsUpdate/diamondData2',pattern = '.csv', full.names = T)
y <- pbapply::pblapply(x, read.csv)
z <- do.call(rbind, y)
z$X <- NULL
z$priceClean <- as.numeric(gsub('[$]|,','',z$price))
z <- z %>% drop_na(priceClean)
write.csv(z,'~/Desktop/DataMiningCases/diamondsUpdate/diamonds2023.csv', row.names = F)

# Image Download
for(i in 12099:nrow(z)){ #12099
  waitTime <- sample(seq(0,.35, by =0.05),1)
  cat(paste('wait time: ',waitTime, 'working on number:',i,'of', length(x),'\n'))
  Sys.sleep(waitTime)
  oneDiamond <- z[i,]
  if(grepl('images?',oneDiamond$imgURL)==F){ 
    print(paste('ERROR:',i))
    errorLog <- paste0('error downloading ',oneDiamond$SKU, 'at url ',oneDiamond$imgURL)
    writeLines(errorLog,  nam <- paste0('~/Desktop/DataMiningCases/diamondsUpdate/diamondImgs/',oneDiamond$SKU, '.txt'))
  } else {
    nam <- paste0('~/Desktop/DataMiningCases/diamondsUpdate/diamondImgs/',oneDiamond$SKU, '.jpg')
    download.file(oneDiamond$imgURL,nam, mode = 'wb')
  }
}



# End