#' Author: Ted Kwartler
#' Date: Feb-17-23
#' Purpose: Build a powerpoint easily

# libraries
library(officer)
library(ggplot2)
library(ggthemes)
library(flextable)
library(dplyr)

# Use during initial construction but not needed in execution
# Custom helper function to keep it all straight
printAllLayouts <- function(pptxObj){
  allLayouts <- layout_summary(pptxObj)
  for(i in 1:length(allLayouts$layout)){
    plot_layout_properties(pptxObj, layout = allLayouts$layout[i], master = NULL, labels = TRUE)
    title(allLayouts$layout[i])
  }
}

# set wd
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Inputs
author        <- 'Dale the Data Scientist'
searchPattern <- 'Wk1' #one could programatically obtain the week of the year using lubridate
dataPaths     <- '~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/DD1/C_Mar6/data'
templatePath  <- '~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/DD1/C_Mar6/data/hult.pptx'

# ID the CSV
pth <- list.files(path = dataPaths,
                  pattern = searchPattern, 
                  full.names = T)
pth <- pth[grep('*.csv', pth)]
df  <- read.csv(pth)

# Create an empty powerpoint
pptx <- read_pptx(templatePath)
layout_summary(pptx)
printAllLayouts(pptx)

# Let's start with a title slide
# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Title Slide", 
                  master = "Hult") # could declare this as an input or with pptx layout summary value

# Programatically declare the title & add
titleTxt <- paste(Sys.Date(), 'Patient Review')
pptx <- ph_with(pptx, value = titleTxt, location = ph_location_label(ph_label = "Title 1"))
pptx <- ph_with(pptx, value = author, location = ph_location_label(ph_label = "Subtitle 2"))

# Add another slide "Comparison" & content
pptx <- add_slide(pptx, 
                  layout = "Comparison", 
                  master = "Hult")
pptx <- ph_with(pptx, value = 'Averages & Distributions', location = ph_location_label(ph_label = "Title 1"))

# Make a simple visual and add it to the first body section
pl   <- ggplot(data = df) + geom_histogram(aes(age)) + theme_wsj()
pptx <- ph_with(pptx, pl, location = ph_location_label( ph_label = "Content Placeholder 3"))

# 2nd box add table of info
avgLabs <- mean(df$num_lab_procedures)
avgAge  <- mean(df$age) 
females <- as.matrix(table(df$gender))[1,1]
males   <- as.matrix(table(df$gender))[2,1]

highlevelDF <- data.frame(AVGlabs = avgLabs,
                          AVGage  = avgAge,
                          Females = females,
                          Males   = males)

# Flextable
ft   <- flextable(data = highlevelDF)
pptx <- ph_with(pptx, value = ft,location = ph_location_label( ph_label = "Content Placeholder 5"))

# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Comparison",
                  master = "Hult")
pptx <- ph_with(pptx, 
                value = 'Readmissions & Medication Info Among Patients', 
                location = ph_location_label(ph_label = "Title 1"))

# Left side subtitle
pptx <- ph_with(pptx, 
                value = 'Readmissions in last 30days', 
                location = ph_location_label(ph_label = "Text Placeholder 2"))

# Left side content
pl<- ggplot(data = df, aes(x = time_in_hospital)) + geom_histogram() + theme_wsj()
pptx <- ph_with(pptx, value = pl, location = ph_location_label(ph_label = "Content Placeholder 3"))

# Right side subtitle
pptx <- ph_with(pptx, 
                value = 'Distribution of num_medications', 
                location = ph_location_label(ph_label = "Text Placeholder 4"))
# Right side content
pl<- ggplot(data = df, aes(x = num_medications)) + 
  geom_density(color = 'maroon') + 
  theme_wsj()
pptx <- ph_with(pptx, 
                value = pl, 
                location = ph_location_label(ph_label = "Content Placeholder 5"))

# Add a programmatic caption in a new slide
pptx <- add_slide(pptx, 
                  layout = "Two Content",
                  master = "Hult")
pptx <- ph_with(pptx, 
                value = 'Correlation between lab procedure & medications', 
                location = ph_location_label(ph_label = "Title 1"))

# Add image
pl <- ggplot(data = df, aes(x= num_medications, y = time_in_hospital, color = readmitted)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~readmitted) + 
  theme_solarized() + 
  theme(legend.position = "none") 
pptx <- ph_with(pptx, value = pl, location = ph_location_label(ph_label = "Content Placeholder 2"))

# Define caption
corTable <- df %>%
  group_by(readmitted, gender) %>%
  summarize(cor=cor(num_medications, time_in_hospital))
corTable$lan <- ifelse(corTable$readmitted==F,'discharged','readmitted')
corTable$cor <- round(corTable$cor,3)
caption <- paste('Among',corTable$lan , corTable$gender,'patients, the correlation between the number of medications and time in hospitals is',corTable$cor)
pptx <- ph_with(pptx, value = caption, location = ph_location_label(ph_label = "Content Placeholder 3"))
####


fileName <- paste0(getwd(),'/diabetesReadout_',searchPattern,'.pptx')
print(pptx, target = fileName)
# End