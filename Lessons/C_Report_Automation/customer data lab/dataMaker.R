# Function to create customer information for lab 1
grpN <- 10
pth <- "~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/DD1/A_Feb27/lab"
makeFakeCuData <- function(cuN      = 10000, 
                           nSKUs    = 100, 
                           minPrice = 5, 
                           maxPrice = 50, 
                           transactionN = 1000000,#conjurer::getTrans
                           outliers = T,#conjurer::getTrans
                           monthSpike = 12, #conjurer::getTrans
                           peakN = 'y', #'y'=1x/yr, 'q'=4x/yr, 'm'= 12x/yr
                           startDate = "2022-01-01", #transaction window
                           endDate = "2022-12-31", #transaction window
                           paretoHighCu = 80, #customer allocation ie 80% of transactions allocated to 20% of customers 
                           paretoLowCu = 20, #customer allocation ie 80% of transactions allocated to 20% of customers
                           paretoHighSKU = 70,#SKU allocation ie 70% of transactions allocated to 30% of the products offered 
                           paretoLowSKU = 30,#SKU allocation ie 70% of transactions allocated to 30% of the products offered 
                           augment = T, # add more customer info from synthetic table
                           productHierarchy = data.frame(category = rep(c('Food','Non-Food')),
                                                         subcategory = c('Beverages','Cleaning','Dairy','Household'))){
  require(conjurer)
  require(generator)
  require(lubridate)
  require(dplyr)
  
  # Build the customers
  customers <- buildCust(numOfCust =  cuN)
  
  # Create a non normal age distro
  betas  <-rbeta(cuN,2,2)
  ageism <- c(betas[1:(cuN/2)]*18,
              betas[(cuN/2+1):cuN]*62)
  ageism <-ageism+18 #enforce a min age but keep the distribution
  
  # Age and phone numbers
  customer2age       <- data.frame(customer = customers, age =  ageism)
  customer2age$phone <-  r_phone_numbers(nrow(customer2age), use_hyphens = T, use_parentheses = T)
  
  # Build out the product catalog and transactions
  products     <- buildProd(numOfProd = nSKUs, minPrice = minPrice, maxPrice = maxPrice)
  transactions <- genTrans(cycles = "y", spike = monthSpike, outliers = outliers, transactions = transactionN)
  
  # Add proper hms not just dayNum from conjurer
  rDate <- function(sDate, eDate, nDate){   
    lenDate <- nDate
    seqDays <- seq.POSIXt(as.POSIXct(sDate), as.POSIXct(eDate), by="secs")  
    aDay <- runif(lenDate, 1, length(seqDays))  
    Date <- seqDays[aDay]  
  }
  
  # Change to the timestamp
  synthTime <- rDate(startDate, endDate, nrow(transactions))
  synthTime <- sort(synthTime)
  
  # Match original dayNum/mthNum append the hms
  transactions$timestamp <- synthTime
  
  # Customer Transaction Allocation
  customer2transaction <- buildPareto(customers, transactions$transactionID, pareto = c(paretoHighCu,paretoLowCu))
  names(customer2transaction) <- c('transactionID', 'customer')
  
  # Product Type Allocation
  products <- data.frame(products, productHierarchy)
  product2transaction <- buildPareto(products$SKU,transactions$transactionID,pareto = c(paretoHighSKU,paretoLowSKU))
  names(product2transaction) <- c('transactionID', 'SKU')
  
  # Create Augmented Table
  if(augment==T){
    aug <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/C_Mar6/customer%20data%20lab/augmentDataTable.csv')
    if(cuN>nrow(aug)){stop(paste('there are only 15k rows in the augmented table but you have cuN:', cuN))}
    idx <- sample(1:nrow(aug), cuN)
    aug <- data.frame(customer = customers, aug[idx,])
  }
 
  
  # Final assembly; could be improved but its ok
  finalDF <- left_join(product2transaction, customer2transaction, by = "transactionID")
  finalDF <- left_join(finalDF, transactions, by = "transactionID")
  finalDF <- left_join(finalDF, customer2age,  by = "customer")
  finalDF <- left_join(finalDF, products, by = "SKU")
  if(augment==T){ finalDF <- left_join(finalDF, aug, by = "customer")}
 
  # Final Ordering
  finalDF$timestamp <- parse_date_time(finalDF$timestamp,"y m d HMS")
  finalDF <- finalDF[order(finalDF$timestamp),]
  return(finalDF)
}

# Individual Run as CSV
#x <- makeFakeCuData(cuN = 2500, transactionN = 100000) #smaller
#write.csv(x, paste0(pth,'/grpN_',grpN, '_fakeCustomerData.csv'), row.names = F)

# Make files for multiple groups and save, FST is more efficient on disk
#for(i in 1:grpN){
#  print(i)
#  x <- makeFakeCuData(cuN = 2500, transactionN = 100000) #smaller
#  fst::write_fst(x, paste0(pth,'/grpN_',i, '_fakeCustomerData.fst'))
#}

# End