#' Author: Ted Kwartler
#' Date: Feb-17-23
#' Purpose: Examining the officer library
#' https://ardata-fr.github.io/officeverse/officer-for-powerpoint.html

# libraries
library(officer)
library(ggplot2)
library(ggthemes)

# Save Path
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Custom helper function to keep it all straight
printAllLayouts <- function(pptxObj){
  allLayouts <- layout_summary(pptxObj)
  for(i in 1:length(allLayouts$layout)){
    plot_layout_properties(pptxObj, layout = allLayouts$layout[i], master = NULL, labels = TRUE)
    title(allLayouts$layout[i])
  }
}


# Create an empty powerpoint
pptx <- read_pptx()

# What type of slides can we make?
layout_summary(pptx)

# Take a look at all the template layouts
printAllLayouts(pptx)

# Save the theme
theme <- 'Office Theme'

# What is the layout for a comparison slide?
layout_properties(x = pptx, 
                  layout = "Comparison", 
                  master = theme) 

# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Comparison", 
                  master = theme)
# Add the title
pptx <- ph_with(pptx, 
                value = "Ted Slide Title", 
                location = ph_location_label( ph_label = "Title 1"))


# Right side title & body
pptx <- ph_with(pptx, 
                value = "Right hand sub-header", 
                location = ph_location_label( ph_label = "Text Placeholder 4"))
pptx <- ph_with(pptx, 
                value = "Right hand content", 
                location = ph_location_label( ph_label = "Content Placeholder 5"))

# Left side title & body
pptx <- ph_with(pptx, 
                value = "Left hand sub-header", 
                location = ph_location_label( ph_label = "Text Placeholder 2"))
pptx <- ph_with(pptx, 
                value = "Left hand content", 
                location = ph_location_label( ph_label = "Content Placeholder 3"))

# Add another slide; 1st check the layout labels
layout_summary(pptx)
layout_properties(x = pptx, 
                  layout = "Title and Content", 
                  master = theme)
pptx <- add_slide(pptx, 
                  layout = "Title and Content", 
                  master = theme)

# Add the title & content
pptx <- ph_with(pptx, 
                value = "title slide 2", 
                location = ph_location_label( ph_label = "Title 1"))
pptx <- ph_with(pptx, 
                value = head(iris), 
                location = ph_location_label( ph_label = "Content Placeholder 2"))

# Add another slide; check the layout & labels
layout_summary(pptx)
layout_properties(x = pptx, 
                  layout = "Two Content", 
                  master = theme)
pptx <- add_slide(pptx, 
                  layout = "Two Content", 
                  master = theme)

p <- ggplot(data = mtcars, aes(x=mpg)) + 
  geom_density(color='red') + 
  theme_gdocs()
summaryInfo <- as.data.frame(as.matrix(summary(mtcars$mpg)))
pptx <- ph_with(pptx, 
                value = "mtcars mpg stats", 
                location = ph_location_label( ph_label = "Title 1"))
pptx <- ph_with(pptx, 
                value = p, 
                location = ph_location_label( ph_label = "Content Placeholder 2"))
pptx <- ph_with(pptx, 
                value = summaryInfo, 
                location = ph_location_label( ph_label = "Content Placeholder 3"))

# Save a copy by "printing" it.
fileName <- paste0(Sys.Date(),'_simpleExample.pptx')
print(pptx, target = fileName)

# End