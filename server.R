library(shiny)
library(ggplot2)
library(tidyverse)
yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
mutate(injuries = as.numeric(gsub(",", "", injuries)))


function(input, output) {
  output$yearly_injuries_by_sport <- renderPlot({
    yearly_injuries_final %>%
      arrange(injuries) %>%
      filter(year == input$year) %>%
      mutate(sport_or_activity = factor(sport_or_activity, levels = sport_or_activity, ordered = TRUE)) %>%
      ggplot(aes(x = sport_or_activity, y = injuries)) + geom_col() + 
      scale_y_continuous(labels = scales::comma) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}
