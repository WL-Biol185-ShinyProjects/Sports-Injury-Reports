library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(bslib)




yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
  
#Fixing the Table
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws(sport_or_activity)) %>%
  group_by(sport_or_activity, year) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")


function(input, output) {
#yearly injuries by year
  output$yearly_injuries_by_sport <- renderPlotly({
    
    data <- yearly_injuries_final %>%
      arrange(injuries) %>%
      filter(year == input$year)
    
    if(nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No data available for this year", 
                 size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- data %>%
      mutate(sport_or_activity = factor(sport_or_activity, 
                                        levels = sport_or_activity, 
                                        ordered = TRUE)) %>%
      ggplot(aes(x = sport_or_activity, 
                 y = injuries,
                 fill = sport_or_activity,        # ADD THIS
                 text = paste("Sport:", sport_or_activity,
                              "<br>Injuries:", scales::comma(injuries)),
                 )) + 
      geom_col(fill = "#78c28d") +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0,0.05))) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            plot.margin = margin(b = 100, r = 20, t = 20, l = 20, unit = "pt"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 1000)
  })
  
#injuries by age
  output$yearly_injuries_by_age <- renderPlotly({
    
    tidyAge <- function(table) {
      tidy <-gather(table,
                    key = "age_group",
                    value = "injuries",
                    '0_to_4':'65_or_over')
    }
    yearly_injuries_by_age <-tidyAge(yearly_injuries_final)
    
    
    
  })
}
