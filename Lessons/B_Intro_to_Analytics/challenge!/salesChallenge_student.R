#' Author: Ted Kwartler
#' Date: Jan 18, 2024
#' Purpose: EDA on a sales dataset
#' Dataset origination: https://www.kaggle.com/datasets/dhruvkothari19/practice-eda-on-this-sales-dataset?resource=download
#' 
#' Possible questions to answer, but many more are possible!
#' What was the best month for sales? How much was earned that month?  Can you make a line plot of this?
#' What city sold the most product? Can you plot the top 6 cities as a bar chart?  You will need to stringplit the Purchase.Address by comma and extract the states and cities.  Maybe look up how to map it too!
#' What time should we display advertisements to maximize the likelihood of customers buying products? Maybe check for the most useful hour of the day?
#' What products are most often sold together? This one is HARD and you could use gpt or the library arules to help.
#' What product sold the most and plot the top 6? table() could be helpful
#' 

# Libraries ggplot, ggtheme, lubridate & turn off scientific notation
library()
library()
library()
options( = )

# Read in the data
salesData <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Lessons/B_Intro_to_Analytics/challenge!/salesData.csv')

# Get the structure of the data frame, the summary of the data & the top 6 rows
___()
_______()
____()

# change Price.Each column to numeric
___$___ <- __._______(___$___)

# Make sure the number ordered is numeric
___$___ <- __._______(___$___)

# Density plot of Price.Each using a layer called geom_density() 
# Make it theme_gdocs() and with a ggtitle
ggplot(data = ___, aes(x = ___)) + 
  _________() +
  _________() +
  _________("Distribution of Unit Price")

# Tally the product column
prodTally <- as.data.frame(_____(___$___))

# Review
head(prodTally)

# Order the tally the ROWS with order() applied to the Freq column
prodTally <- ___[____(___$___, decreasing = T),]

# Select the top 5 products 2 different ways
topFive <- prodTally[_:_,]
topFive <- ____(prodTally, _)

# Create a geom_col chart with theme_few(), & a ggtitle()
ggplot(data = _____, aes(x = ____, y = ____)) +
  geom_col() + _______() +
  _______('top 5 product sales') + 
  theme(axis.text.x = element_text(angle = 90))

# Engineer variables from days; just know these exist
salesData$Order.Date  <- mdy_hm(salesData$Order.Date) #overwrite as a data object
salesData$dayOfWeek   <- wday(salesData$Order.Date)
salesData$dayOfMonth  <- mday(salesData$Order.Date)
salesData$weekday     <- weekdays(salesData$Order.Date)
salesData$hourOfDay   <- hour(salesData$Order.Date)
salesData$dayOfYear   <- yday(salesData$Order.Date)
salesData$month       <- month(salesData$Order.Date)
salesData$year        <- year(salesData$Order.Date)


# Examine a portion of the data

# Tally (table) the year column to see if there is any data skew

# Subset the data to just "2019" 
# hint: salesData <- subset(data object, column name == 2019) 
# *remember the double ==* 

# Let's aggregate() up Price by month and sum
monthlySales <- _______(_____ ~ _____, data = ____, ___)

# Change to month name; just know R has built in months variables
monthlySales$month <-  month.name[monthlySales$month]

# Find maximum month using which.max() on the price each column within the rows side of the comma
______[_______(____$___),]

# Data prep for visual; ggplot needs factors otherwise characters will be alphabetical
monthlySales$month <- factor(monthlySales$month, levels = month.name)

# Plot monthlySales month by price each as a geom_line() with 
ggplot(data = _______, aes(x = ____, y = ____,  group = 1)) + 
  _______() + 
  scale_x_discrete(limits = month.name) + 
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle('Sales by month')

# Now your turn.  Try to answer some of the questions or explore on your own!



# End