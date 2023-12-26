#' Author: Ted Kwartler
#' Date: 2-19-2023
#' Purpose: Build a regression model
#' 

# libs
library(ggplot2)
library(ggthemes)
library(dplyr)

# Data
diamonds <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1/E_Mar9/data/diamonds2023.csv')
# an older version of this data set can be loaded with ggplot, names are different but results are similar
#data(diamonds)

# EDA
summary(diamonds)

# Let's see if there are outliers
ggplot(data = diamonds, aes(y=priceClean)) + geom_boxplot() + theme_gdocs()

# Let's drop the outliers in the top demi decile
quantile(diamonds$priceClean, probs = seq(.1,.95, by = .05))
dropAmt <- tail(quantile(diamonds$priceClean, probs = seq(.1,.95, by = .05)), 1)
diamonds <- subset(diamonds, diamonds$priceClean<dropAmt)

# Build a scatter plot to show relationship 
p <- ggplot(diamonds, aes(Carat, priceClean)) + geom_point(alpha=0.02) + theme_gdocs()
p

# Since we see a relationship let's make a linear model to predict prices
fit <- lm(priceClean ~ Carat + 0, diamonds)
fit

# Add out model predictions
p <- p + geom_abline(intercept =  0, 
                     slope = coefficients(fit), 
                     color='red') +
  theme_gdocs()
p

# End
