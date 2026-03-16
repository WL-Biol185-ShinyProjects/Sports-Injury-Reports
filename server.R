library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(bslib)
library(ggpie)




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

agegroup_percentage <-read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws (sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
  ) %>%
  group_by(sport_or_activity, year, X0_to_4, X4_to_15, X14_to_24, X25_to_64, X65_or_over, injuries) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

  agegroup_percentage$percent_0_to_4 <- agegroup_percentage$X0_to_4/agegroup_percentage$injuries
  agegroup_percentage$percent_4_to_15 <- agegroup_percentage$X4_to_15/agegroup_percentage$injuries
  agegroup_percentage$percent_14_to_24 <- agegroup_percentage$X14_to_24/agegroup_percentage$injuries
  agegroup_percentage$percent_25_to_64 <- agegroup_percentage$X25_to_64/agegroup_percentage$injuries
  agegroup_percentage$percent_65_or_over <- agegroup_percentage$X65_or_over/agegroup_percentage$injuries

function(input, output, session) {
  
  output$tab_title <-renderUI({
    if (input$tabs =="Home") return(NULL)
    
    div(
      h3(input$tabs),
      style = "text-align: center; color: #2c3e50; font-weight: 600;
               border-bottom: 2px solid #e0e0e0; padding-bottom: 10px;
               margin-bottom: 20px;"
    )
  })
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
  })
  
  #sport injuries by age
  output$sport_injuries_by_age <- renderUI({
    req(input$sport_or_activity)
    
    plot_output_list <- lapply(seq_along(input$sport_or_activity), function(i) {
      plotOutput(paste0("pie_", i), height = "300px")
    })
    
    do.call(fluidRow, lapply(plot_output_list, function(p) column(12 / length(input$sport_or_activity), p)))
  })
  
  observe({
    req(input$sport_or_activity)
    
    lapply(seq_along(input$sport_or_activity), function(i) {
      local({
        sport_name <- input$sport_or_activity[i]
        plot_id    <- paste0("pie_", i)
        
        output[[plot_id]] <- renderPlot({
          df <- agegroup_percentage %>%
            filter(sport_or_activity == sport_name) %>%
            summarise(
              "0 to 4"    = sum(percent_0_to_4,     na.rm = TRUE),
              "4 to 15"   = sum(percent_4_to_15,    na.rm = TRUE),
              "14 to 24"  = sum(percent_14_to_24,   na.rm = TRUE),
              "25 to 64"  = sum(percent_25_to_64,   na.rm = TRUE),
              "65 or over"= sum(percent_65_or_over, na.rm = TRUE)
            ) %>%
            pivot_longer(cols = everything(),
                         names_to  = "age_group",
                         values_to = "pct") %>%
            mutate(pct = pct / sum(pct) * 100)
          
          ggplot(df, aes(x = "", y = pct, fill = age_group)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y") +
            labs(title = sport_name, fill = "Age Group") +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            geom_text(aes(label = paste0(round(pct, 1), "%")),
                      position = position_stack(vjust = 0.5), size = 3)
        })
      })
    })
  })
  
  
  
  
  }

