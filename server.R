library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(bslib)
library(ggrepel)
library(scales)
library(leaflet)


yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws(sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
  ) %>%
  group_by(sport_or_activity, year) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

injuries_by_agegroup <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws(sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
  ) %>%
  group_by(sport_or_activity, year, X0_to_4, X4_to_15, X14_to_24, X25_to_64, X65_or_over, injuries) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

agegroup_percentage <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws(sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
  ) %>%
  group_by(sport_or_activity, year, X0_to_4, X4_to_15, X14_to_24, X25_to_64, X65_or_over, injuries) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

agegroup_percentage$percent_0_to_4    <- agegroup_percentage$X0_to_4    / agegroup_percentage$injuries
agegroup_percentage$percent_4_to_15   <- agegroup_percentage$X4_to_15   / agegroup_percentage$injuries
agegroup_percentage$percent_14_to_24  <- agegroup_percentage$X14_to_24  / agegroup_percentage$injuries
agegroup_percentage$percent_25_to_64  <- agegroup_percentage$X25_to_64  / agegroup_percentage$injuries
agegroup_percentage$percent_65_or_over <- agegroup_percentage$X65_or_over / agegroup_percentage$injuries

favorite_sport_by_state <- read.csv("favorite_sport_by_state.csv") %>%
  mutate(state = trimws(state),
         state = recode(state,
                        "colorodo"       = "colorado",
                        "conneticut"     = "connecticut",
                        "illinios"       = "illinois",
                        "massachusets"   = "massachusetts",
                        "minnisota"      = "minnesota",
                        "new_hampshire"  = "new hampshire",
                        "new_jersey"     = "new jersey",
                        "new_mexico"     = "new mexico",
                        "new_york"       = "new york",
                        "north_carolina" = "north carolina",
                        "north_dakota"   = "north dakota",
                        "rhode_island"   = "rhode island",
                        "south_carolina" = "south carolina",
                        "south_dakota"   = "south dakota",
                        "tenessee"       = "tennessee",
                        "west_virginia"  = "west virginia"))
function(input, output, session) {
  
  output$tab_title <- renderUI({
    if (input$tabs == "Home") return(NULL)
    div(
      h3(input$tabs),
      style = "text-align: center; color: #2c3e50; font-weight: 600;
               border-bottom: 2px solid #e0e0e0; padding-bottom: 10px;
               margin-bottom: 20px;"
    )
  })
  
  # Sport Injuries Per Year
  output$yearly_injuries_by_sport <- renderPlotly({
    yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
      mutate(injuries = as.numeric(gsub(",", "", injuries)),
             year = as.numeric(year),
             sport_or_activity = trimws(sport_or_activity),
             sport_or_activity = stringr::str_squish(sport_or_activity),
      ) %>%
      group_by(sport_or_activity, year) %>%
      summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")
    
    data <- yearly_injuries_final %>%
      arrange(injuries) %>%
      filter(year == input$year)
    
    if (nrow(data) == 0) {
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
                 text = paste("Sport:", sport_or_activity,
                              "<br>Injuries:", scales::comma(injuries)))) +
      geom_col(fill = "#78c28d") +
      xlab("Sport or Activity") +
      ylab("Numbers of Injuries") +
      ggtitle("Sport Injuries Per Year") +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.05))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            plot.margin = margin(b = 100, r = 20, t = 20, l = 20, unit = "pt"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 1000)
  })
  
  # Injuries by Age Group
  output$yearly_injuries_by_age <- renderPlot({
    tidyAge <- function(table) {
      gather(table, key = "age_group", value = "injuries", "X0_to_4":"X65_or_over")
    }
    yearly_injuries_by_age <- tidyAge(injuries_by_agegroup)
    
    yearly_injuries_by_age %>%
      filter(age_group == input$age_group,
             sport_or_activity %in% input$sport_or_activity) %>%
      group_by(year, sport_or_activity) %>%

      summarise(injuries = sum(injuries, na.rm = TRUE), .group = "drop") %>%
      ggplot(aes(x=year,
                 y=injuries,
                 color=sport_or_activity,
                 group = sport_or_activity
                ))+
      geom_line()+
      geom_point()+
      scale_x_continuous(breaks = unique(yearly_injuries_by_age$year))+
      xlab("Year")+
      ylab("Number of Injuries")+
      ggtitle("Injuries by Age Group Over Time")+
      labs(color = "Sport") +
      theme(
        plot.title   = element_text(size = 22),
        axis.title   = element_text(size = 17),
        axis.text    = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text  = element_text(size = 15)
      )
  
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
  
  observeEvent(input$go_state, {
    updateTabsetPanel(session, "tabs", selected = "Favorite Sport by State")
  })
  
  # Sport Injuries by Age (pie charts)
  output$sport_injuries_by_age <- renderUI({
    req(input$sport_or_activity)
    plot_output_list <- lapply(seq_along(input$sport_or_activity), function(i) {
          plotOutput(paste0("pie_", i), width = "450px", height = "450px")
    })
    
    div(style = "display: flex; flex-wrap: wrap;",
        tagList(plot_output_list))

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
              "0 to 4"     = sum(percent_0_to_4,     na.rm = TRUE),
              "4 to 15"    = sum(percent_4_to_15,    na.rm = TRUE),
              "14 to 24"   = sum(percent_14_to_24,   na.rm = TRUE),
              "25 to 64"   = sum(percent_25_to_64,   na.rm = TRUE),
              "65 or over" = sum(percent_65_or_over, na.rm = TRUE)
            ) %>%
            pivot_longer(cols = everything(),
                         names_to  = "age_group",
                         values_to = "pct") %>%
            mutate(pct = pct / sum(pct) * 100)
          
          df <- df %>% arrange(desc(age_group))
          ggplot(df, aes(x = "", y = pct, fill = age_group)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y") +
            labs(title = sport_name, fill = "Age Group") +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 25),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 20)) +
            geom_text_repel(aes(label = paste0(age_group, "\n", round(pct, 1), "%"),
                                y = cumsum(pct) - 0.5 * pct),
                            nudge_x = 0.7,
                            show.legend = FALSE,
                            size = 6)
        })
      })
    })
  })
  
  # Favorite Sport by State Map
  output$sport_map <- renderLeaflet({
    state_coords <- data.frame(
      state = tolower(state.name),
      lat   = state.center$y,
      lon   = state.center$x
    )
    
    map_data <- left_join(favorite_sport_by_state, state_coords, by = "state")
    
    sport_colors <- c(
      "NFL_Football"       = "#78c2ad",
      "NBA_Basketball"     = "#f3969a",
      "MLB_Baseball"       = "#56cc9d",
      "NHL_Hockey"         = "#6cc3d5",
      "College_Football"   = "#ffce67",
      "College_Basketball" = "#ff7851"
    )
    
    pal <- colorFactor(palette = sport_colors, domain = names(sport_colors))
    
    leaflet(data = map_data) %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng          = ~lon,
        lat          = ~lat,
        color        = "white",
        fillColor    = ~pal(favorite_sport),
        fillOpacity  = 0.9,
        weight       = 2,
        radius       = 18,
        stroke       = TRUE,
        label        = ~paste0(stringr::str_to_title(state), ": ", gsub("_", " ", favorite_sport)),
        labelOptions = labelOptions(
          style = list(
            "font-weight"      = "bold",
            "font-size"        = "14px",
            "background-color" = "#78c2ad",
            "color"            = "white",
            "border"           = "none",
            "border-radius"    = "4px",
            "padding"          = "4px 8px"
          )
        ),
        popup        = ~paste0(
          "<div style='max-width:300px; font-family:Arial; padding:5px;'>",
          "<h3 style='color:#78c2ad; margin:0 0 5px 0; border-bottom: 2px solid #78c2ad; padding-bottom:5px;'>",
          stringr::str_to_title(state), "</h3>",
          "<div style='text-align:center; margin:10px 0;'>",
          case_when(
            favorite_sport == "NFL_Football"       ~ "<img src='https://upload.wikimedia.org/wikipedia/en/a/a2/National_Football_League_logo.svg' height='60px'>",
            favorite_sport == "NBA_Basketball"     ~ "<img src='https://cdn.freebiesupply.com/logos/large/2x/nba-logo-png-transparent.png' height='60px'>",
            favorite_sport == "MLB_Baseball"       ~ "<img src='https://www.mlbstatic.com/team-logos/league-on-dark/1.svg' height='60px'>",
            favorite_sport == "NHL_Hockey"         ~ "<img src='https://upload.wikimedia.org/wikipedia/en/3/3a/05_NHL_Shield.svg' height='60px'>",
            favorite_sport == "College_Football"   ~ "<img src='https://upload.wikimedia.org/wikipedia/commons/d/dd/NCAA_logo.svg' height='60px'>",
            favorite_sport == "College_Basketball" ~ "<img src='https://upload.wikimedia.org/wikipedia/commons/d/dd/NCAA_logo.svg' height='60px'>",
            TRUE ~ ""
          ),
          "</div>",
          "<p style='background:#f8f9fa; padding:5px 8px; border-radius:4px;'>",
          "<b>Favorite Sport:</b> ", gsub("_", " ", favorite_sport), "</p>",
          "<p style='font-size:12px; color:#555; line-height:1.5;'>",
          case_when(
            favorite_sport == "NBA_Basketball"     ~ "According to a 17-year study published in PMC, lateral ankle sprains are the most common NBA injury at 13.2%, followed by patellofemoral (knee) inflammation at 11.9%, lumbar strains at 7.9%, and hamstring strains at 3.3%. The lower extremity overall accounts for 62.4% of all injuries and is responsible for 72.3% of games missed.",
            favorite_sport == "College_Football"   ~ "Injuries to the lower extremity were most common, constituting 50% of all injuries. The proportion of injuries to other anatomic areas was 21% for the head/neck, 15% for the upper extremity, and 14% for the trunk/back. Injuries were attributed to contact with another player in 59% of cases, noncontact in 32% of cases, and an unknown mechanism in 9% of cases.",
            favorite_sport == "NFL_Football"       ~ "In general, the offensive lineman positions sustain the highest number of injuries while the running back has the highest rate. Overall, the knee was the most commonly injured site followed by the ankle.",
            favorite_sport == "MLB_Baseball"       ~ "Baseball players may experience a range of arm-related injuries involving the shoulder, elbow, hand or wrist due to overuse caused by repetitive throwing and bat swinging.",
            favorite_sport == "NHL_Hockey"         ~ "In the NHL body checking made up 28.6% of injuries. Only 6.2% of injuries were sustained by goaltenders, whereas 32.7% were defensemen and 61.1% were forwards.",
            favorite_sport == "College_Basketball" ~ "According to NCAA injury surveillance data, ankle sprains are the most common college basketball injury at 16.2%, followed by concussions at 4.6%.",
            TRUE ~ ""
          ),
          "</p></div>"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~favorite_sport, title = "Favorite Sport")
  })
  # Summary cards
  output$most_popular_sport <- renderText({
    favorite_sport_by_state %>%
      count(favorite_sport) %>%
      slice_max(n, n = 1) %>%
      pull(favorite_sport) %>%
      gsub("_", " ", .)
  })
  
  output$num_sports <- renderText({
    favorite_sport_by_state %>%
      distinct(favorite_sport) %>%
      nrow()
  })
  
  output$num_states <- renderText({
    nrow(favorite_sport_by_state)
  })
  
  # Bar chart
  output$sport_state_bar <- renderPlot({
    favorite_sport_by_state %>%
      count(favorite_sport) %>%
      mutate(favorite_sport = gsub("_", " ", favorite_sport)) %>%
      arrange(desc(n)) %>%
      ggplot(aes(x = reorder(favorite_sport, n), y = n, fill = favorite_sport)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#78c2ad", "#f3969a", "#56cc9d",
                                   "#6cc3d5", "#ffce67", "#ff7851")) +
      coord_flip() +
      xlab("") +
      ylab("Number of States") +
      ggtitle("How Many States Prefer Each Sport?") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}