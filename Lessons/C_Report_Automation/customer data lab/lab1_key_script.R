#' Ted Kwartler
#' R Data Viz: Lab 1
#' 

# Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)

# Read in the Data
#cuDF <- readr::read_csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/A_Feb27/lab/grpN_10_fakeCustomerData.csv')

# OR Construct the data with different distributions
sourceURL <- "https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/A_Feb27/lab/dataMaker.R"
devtools::source_url(sourceURL)
prods <- data.frame(category    = rep(c('Food','Non-Food')),
                    subcategory = c('Beverages','Sanitary','Dairy','Household'))
cuDF <- makeFakeCuData(cuN              = 10000, 
                       nSKUs            = 100, 
                       minPrice         = 5, 
                       maxPrice         = 50, 
                       transactionN     = 1000000,
                       outliers         = T,
                       monthSpike       = 12,
                       peakN            = 'y', 
                       startDate        = "2022-01-01", 
                       endDate          = "2022-12-31", 
                       paretoHighCu     = 80, #80% transaction from 20% of customers
                       paretoLowCu      = 20, 
                       paretoHighSKU    = 70,#70% od SKUs sold from 30% of SKUs
                       paretoLowSKU     = 30,
                       augment          = T, #add more fake variables from other table
                       productHierarchy = prods)

# Examine the data
head(cuDF)
names(cuDF)

## 1. Identify the top 10 states by number of customers. 
# Base R
customerByStateA <- aggregate(data = cuDF,
                              customer ~ state,
                              function(x) length(unique(x)))
customerByStateA <- customerByStateA[order(customerByStateA$customer, decreasing = T)[1:10],]

# dplyr
customerByStateB <- cuDF %>%
  group_by(state) %>%
  summarise(count = n_distinct(customer)) %>%
  slice_max(n= 10, count)

# Now lets plot a bar chart
ggplot(data = customerByStateB, aes(x = reorder(state, -count), y = count)) + 
  geom_col(fill = '#bada55') + 
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("States") + 
  ylab("Number of Customers") + 
  ggtitle('Top States by Number of Customers') +
  # Optional for bar labels
  geom_text(aes(label = count, x = state), vjust = 1.5)

#### 2. Create a stacked bar chart in ggplot2
# Base R
SKUbyStateA <- as.data.frame(table(cuDF$state, cuDF$SKU))
SKUbyStateA <- SKUbyStateA[order(SKUbyStateA$Var1),]
SKUbyStateA <- SKUbyStateA[SKUbyStateA$Freq!= 0, ]

# dplyr
SKUbyStateB <- cuDF %>%
  group_by(state) %>%
  count(SKU)

# Subset to the top 10 states
idx <- SKUbyStateA$Var1 %in% customerByStateA$state
SKUbyStateA <- SKUbyStateA[idx,] # 10 states * 100 SKUs = 1000 rows

# Identify the quartiles and group the rest
SKUbyStateA$quartile <- ntile(SKUbyStateA$Freq, 4)
top <- subset(SKUbyStateA, SKUbyStateA$quartile==4) #top quartile
top <- top[order(top$Freq),]

# Build the plot
ggplot(top, aes(fill=Var2, y=Freq, x=reorder(Var1, -Freq))) + 
  geom_bar(position="stack", stat="identity", color = 'darkgrey', show.legend = F) + 
  geom_text(aes(label = Var2), size = 2, position = position_stack(vjust = 0.5)) + 
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("States") + 
  ylab("SKU Volume") + 
  ggtitle('Top States & Top SKUs') 

## 3. Stacked Percentage bar chart
# Just the top state
stateSpend <- aggregate(Price ~ state, cuDF, sum)
topState   <- stateSpend[which.max(stateSpend$Price),]
lowState   <- stateSpend[which.min(stateSpend$Price),]
plotDF     <- subset(cuDF, cuDF$state==topState$state[1]|cuDF$state==lowState$state[1])

# Plot
ggplot(plotDF, aes(fill=subcategory, y=Price, x=state)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_hc()

## 4. Create a histogram of customer age
# Base R
individuals <- cuDF[!duplicated(cuDF$customer),]

# Plot
ggplot(individuals, aes(x = age)) + 
  geom_histogram(fill = '#c00c00', color = 'darkgrey') +
  theme_fivethirtyeight() +
  ggtitle('Customer Age Distribution') 

## 5. Create a kernel density plot of customer spend 
# Base R 
householdSpend <- aggregate(Price ~ customer, cuDF, sum)

ggplot(householdSpend, aes(x = Price)) + 
  geom_density(fill = '#c0ffee') + 
  theme_few() + 
  ggtitle('Avg Household Spend')

## 6. Build a box plot of each subcategory for the top state by revenue 
# Base R
stateSpend <- aggregate(Price ~ state, cuDF, sum)
topState   <- stateSpend[which.max(stateSpend$Price),]
plotDF     <- subset(cuDF, cuDF$state==topState$state[1])

# Initialize the plot
p <- ggplot(plotDF, aes(x=subcategory, y=Price, fill = subcategory)) + 
  geom_boxplot(position=position_dodge(width=0.8), show.legend = FALSE) + 
  theme_few()

# Get stats
catSpend <- aggregate(Price ~ subcategory, plotDF, mean)
catSpend$Price <- round(catSpend$Price)

# Add label layer & call the updated plot object
p <- p + geom_text(data = catSpend, aes(label = Price),position = position_dodge(width=0.8)) + 
  ggtitle(paste(topState$state, 'Top State Household Spend by Subcategory'))
p

## 7. Create a scatter plot between customer spend and EstHomeValue 
# Sum the price by customer (total spend) within the sample
totalSpendByCu <- aggregate(Price~customer, cuDF, sum)

# Get a sample
set.seed(2023)
idx <- sample(1:nrow(totalSpendByCu), 1000)
sampSpendByCu <- totalSpendByCu[idx,]

# In the original data, drop duplicates, and retain only the 2 columns (join & data)
indCuDF <- cuDF[!duplicated(cuDF$customer), names(cuDF)%in% c('customer','EstHomeValue', 'state')]

# Perform the join by ID
sampSpendByCu <- inner_join(sampSpendByCu, indCuDF, by = "customer")

# Clean up the dollar column
sampSpendByCu$EstHomeValue <- as.numeric(gsub('[$]','',sampSpendByCu$EstHomeValue))

# Make a grouping to show the way to change shapes by group
sampSpendByCu$top10 <- sampSpendByCu$state  %in% customerByStateA$state

# Examine
ggplot(sampSpendByCu, aes(x=EstHomeValue, y=Price, group = top10)) + 
  geom_point(aes(shape=top10, color= top10), alpha = 0.5) +
  theme_calc() + ggtitle('House Value to Spend')

## 9. Plot a line chart of the total revenue by day
# Declare as data class
cuDF$date <- as.Date(cuDF$timestamp)

# Aggregate ID by date, get the length(total number of transactions by day)
plotDF <- aggregate(Price~date, cuDF,sum)

# Plot
ggplot(plotDF, aes(x=date, y=Price)) + 
  geom_line(alpha = 0.3) +
  theme_gdocs() + 
  geom_smooth(method=loess, se=FALSE)

# End