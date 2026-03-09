library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(bslib)




yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws (sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
  ) %>%
  group_by(sport_or_activity, year) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

injuries_by_agegroup <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws (sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
  ) %>%
  group_by(sport_or_activity, year, X0_to_4, X4_to_15, X14_to_24, X25_to_64, X65_or_over, injuries) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

function(input, output, session) {
#Sport Injuries Per Year
  output$yearly_injuries_by_sport <- renderPlotly({
    
    yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
      mutate(injuries = as.numeric(gsub(",", "", injuries)),
             year = as.numeric(year),
             sport_or_activity = trimws (sport_or_activity),
             sport_or_activity = stringr::str_squish(sport_or_activity),
      ) %>%
      group_by(sport_or_activity, year) %>%
      summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")
    
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
      xlab("Sport or Activity")+
      ylab("Numbers of Injuries")+
      ggtitle("Sport Injuries Per Year")+
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0,0.05))) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            plot.margin = margin(b = 100, r = 20, t = 20, l = 20, unit = "pt"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 1000)
  })
  
#Injuries by Age Group
  output$yearly_injuries_by_age <- renderPlot({
    
    tidyAge <- function(table) {
      tidy <-gather(table,
                    key = "age_group",
                    value = "injuries",
                    "X0_to_4":"X65_or_over")
      return(tidy)
    }
    yearly_injuries_by_age <-tidyAge(injuries_by_agegroup)

    yearly_injuries_by_age %>%
      filter(age_group == input$age_group,
             sport_or_activity %in% input$sport_or_activity
             ) %>%
      group_by(year, sport_or_activity) %>%
      summarise(injuries = sum(injuries, na.rm = TRUE), .group = "drop") %>%
      ggplot(aes(x=year,
                 y=injuries,
                 color=sport_or_activity,
                 group = sport_or_activity
                ))+
      geom_line()+
      xlab("Year")+
      ylab("Number of Injuries")+
      ggtitle("Injuries by Age Group Over Time")+
    labs(color = "Sport")
    
  
      
  })
  # Navigate to tabs when buttons are clicked
  observeEvent(input$go_sport, {
    updateTabsetPanel(session, "tabs", selected = "Sport Injuries Per Year")
  })
  
  observeEvent(input$go_age, {
    updateTabsetPanel(session, "tabs", selected = "Injuries by Age Group")
  })
  
  observeEvent(input$go_sport_age, {
    updateTabsetPanel(session, "tabs", selected = "Sport Injuries by Age")
  })}

