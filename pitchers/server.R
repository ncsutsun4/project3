library(shiny)
library(pitchRx)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(knitr)
library(plyr)
library(DT)



# Download data set 
if(!exists("mondayBaseball")) {
    load(url("http://stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/mondayBaseball.Rdata"))
}

# Subset new data set with only pitches hit in play and remove some unrelated varables
InPlay <- mondayBaseball %>% filter(pitchResult == "IP") %>% 
    select(-c(gameString, gameDate, batterId, batterName, batterPosition, timesFaced,
              catcher, umpireId, umpire))

# balls : # of balls thrown before that thrown
# strikes : # of strickes batter obtained
# outs  :  players been declared out
# 



library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
