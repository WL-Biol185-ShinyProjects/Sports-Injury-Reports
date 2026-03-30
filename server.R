library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(bslib)
library(ggrepel)
library(scales)
library(leaflet)
library(ggpubr)



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
  
  output$top_sport_boxes_wrapper <- renderUI({
    yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
      mutate(injuries = as.numeric(gsub(",", "", injuries)),
             year = as.numeric(year),
             sport_or_activity = trimws(stringr::str_squish(sport_or_activity))) %>%
      group_by(sport_or_activity, year) %>%
      summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")
    
    filtered <- yearly_injuries_final %>%
      filter(year == input$year) %>%
      arrange(desc(injuries))
    
    if (nrow(filtered) == 0) return(NULL)
    
    top <- slice(filtered, 1)
    bot <- slice(filtered, n())
    
    div(style = "background: white; border: 1px solid #ddd; border-radius: 8px; padding: 12px; width: 100%;",
        h5("Year at a Glance", style = "text-align: center; color: #555; margin-top: 0;"),
        div(style = "background: #f3969a; border-radius: 8px; padding: 10px; text-align: center; margin-bottom: 10px;",
            p("Most injured", style = "font-size: 11px; color: gray; margin-bottom: 4px;"),
            p(gsub("_", " ", top$sport_or_activity), style = "font-size: 12px; font-weight: bold; color: #a83237; margin: 0;"),
            p(scales::comma(top$injuries), style = "font-size: 16px; font-weight: bold; color: #a83237; margin: 4px 0 0;")
        ),
        div(style = "background: #78c2ad; border-radius: 8px; padding: 10px; text-align: center;",
            p("Least injured", style = "font-size: 11px; color: gray; margin-bottom: 4px;"),
            p(gsub("_", " ", bot$sport_or_activity), style = "font-size: 12px; font-weight: bold; color: #1a5c4a; margin: 0;"),
            p(scales::comma(bot$injuries), style = "font-size: 16px; font-weight: bold; color: #1a5c4a; margin: 4px 0 0;")
        )
    )
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
      ggplot(aes(x = year,
                 y = injuries,
                 color = sport_or_activity,
                 group = sport_or_activity)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
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
    updateTabsetPanel(session, "tabs", selected = "Injuries By Age Group")
  })
  
  observeEvent(input$go_sport_age, {
    updateTabsetPanel(session, "tabs", selected = "Sport Injuries By Age")
  })
  
  observeEvent(input$go_state, {
    updateTabsetPanel(session, "tabs", selected = "Favorite Sport By State")
  })
  
  observeEvent(input$go_prevention, {
    updateTabsetPanel(session, "tabs", selected = "Injury Prevention")
  })
  
  observeEvent(input$go_about, {
  updateTabsetPanel(session, "tabs", selected = "About Us")
  })
  
  
  
  #Sport Injuries by Age
  age_colors <- c(
    "0 to 4"     = "#f3969a",
    "4 to 15"    = "#56cc9d",
    "14 to 24"   = "#6cc3d5",
    "25 to 64"   = "#ff7851",
    "65 or over" = "#ffce67"
  )
  
  output$sport_injuries_by_age <- renderUI({
    req(input$sport_or_activity)
    plot_output_list <- lapply(seq_along(input$sport_or_activity), function(i) {
      plotOutput(paste0("pie_", i), width = "450px", height = "450px")
    })
    
    div(style = "display: flex; flex-wrap: wrap; gap: 30px; padding: 20px; justify-content: center;",
        tagList(plot_output_list))
  })
  
  output$age_legend <- renderPlot({
    df <- data.frame(
      age_group = c("0 to 4", "4 to 15", "14 to 24", "25 to 64", "65 or over"),
      pct = c(20, 20, 20, 20, 20)
    )
    p <- ggplot(df, aes(x = "", y = pct, fill = age_group)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = age_colors) +
      labs(fill = "Age Group") +
      theme_void() +
      theme(legend.position = "right",
            legend.direction = "vertical",
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 15),
            legend.key.size = unit(1, "cm"))
    ggpubr::as_ggplot(ggpubr::get_legend(p))
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
            scale_fill_manual(values = age_colors) +
            labs(title = tools::toTitleCase(
              stringr::str_wrap(
                gsub("_", " ", sport_name), width = 23)),
              fill = "Age Group") +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 25, lineheight = 0.9),
                  legend.position = "none") +
            geom_text_repel(data = . %>% filter(pct > 0),
                            aes(label = ifelse(pct < 1, 
                                               paste0(age_group, "\n", round(pct, 1), "%"),
                                               paste0(round(pct, 1), "%")),
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
    
    pal <- colorFactor(palette = "Set3", domain = map_data$favorite_sport)
    
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
  fun_facts <- c(
    "Basketball and football consistently rank among the top 3 most injury prone sports every year.",
    "Children aged 4 to 15 account for a large share of sports injuries each year.",
    "Overuse injuries make up nearly 50% of all youth sports injuries.",
    "The 25 to 64 age group sees the most cycling related injuries.",
    "Exercise and fitness activity injuries have risen steadily since 2007.",
    "Snow skiing injuries peak in adults aged 25 to 64.",
    "Soccer injuries are most common in the 14 to 24 age group.",
    "Trampolining sends thousands of children to the ER every single year."
  )
  
  fact_index <- reactiveVal(1)
  
  observeEvent(input$next_fact, {
    next_i <- (fact_index() %% length(fun_facts)) + 1
    fact_index(next_i)
  })
  
  output$fun_fact <- renderUI({
    p(style = "font-size: 18px; color: #444; line-height: 1.6; margin: 0;",
      fun_facts[fact_index()])
  })
  hydro_vals <- reactive({
    w <- input$hydro_weight
    d <- input$hydro_duration
    i <- as.numeric(input$hydro_intensity)
    baseline <- round(w * 0.55)
    extra    <- round((d / 15) * 5 * i)
    list(baseline = baseline, extra = extra, total = baseline + extra)
  })
  # Load ACL data
  acl_by_sport <- read.csv("acl_by_sport.csv")
  acl_risk <- read.csv("acl_risk_by_sport_gender.csv")
  notch_data <- read.csv("notch_width_data.csv")
  
  
  output$acl_total_bar <- renderPlot({
    ggplot(acl_by_sport, aes(x = reorder(sport, total_reconstructions),
                             y = total_reconstructions,
                             fill = sport)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#78c2ad", "#f3969a", "#6cc3d5")) +
      geom_text(aes(label = scales::comma(total_reconstructions)),
                hjust = -0.1, size = 4, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      xlab("") +
      ylab("Number of Reconstructions") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 13))
  })
  
  
  output$acl_gender_split <- renderPlot({
    acl_by_sport %>%
      pivot_longer(cols = c(male_pct, female_pct),
                   names_to = "gender",
                   values_to = "pct") %>%
      mutate(gender = recode(gender,
                             "male_pct" = "Male",
                             "female_pct" = "Female")) %>%
      ggplot(aes(x = sport, y = pct, fill = gender)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Female" = "#f3969a", "Male" = "#6cc3d5")) +
      geom_text(aes(label = paste0(pct, "%")),
                position = position_dodge(width = 0.9),
                vjust = -0.4, size = 3.5, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                         labels = function(x) paste0(x, "%")) +
      xlab("") +
      ylab("Percentage") +
      labs(fill = "Gender") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 13),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 11))
  })
  
  
  output$acl_risk_gender <- renderPlot({
    ggplot(acl_risk, aes(x = reorder(sport, risk_pct),
                         y = risk_pct,
                         fill = gender)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Male" = "#6cc3d5", "Female" = "#f3969a")) +
      geom_text(aes(label = paste0(risk_pct, "%")),
                position = position_dodge(width = 0.9),
                hjust = -0.1, size = 3.5, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                         labels = function(x) paste0(x, "%")) +
      xlab("") +
      ylab("% of ACL Reconstructions") +
      labs(fill = "Gender") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 13),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 11))
  })
  
  
  output$acl_notch_width <- renderPlot({
    notch_data %>%
      filter(group == "All") %>%
      ggplot(aes(x = gender, y = notch_width_mm, fill = acl_status)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Normal" = "#78c2ad", "ACL Tear" = "#f3969a")) +
      geom_text(aes(label = paste0(notch_width_mm, " mm")),
                position = position_dodge(width = 0.9),
                vjust = -0.4, size = 4, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                         limits = c(0, 20)) +
      xlab("") +
      ylab("Notch Width (mm)") +
      labs(fill = "ACL Status") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 13),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 11))
  })
  
  
  
  output$acl_prevention <- renderPlot({
    prevention_data <- data.frame(
      strategy = c(
        "Neuromuscular + Strength Training",
        "Multi-Faceted Programs (Female)",
        "Multi-Faceted Programs (Male)",
        "Plyometrics",
        "Balance / Proprioceptive Training"
      ),
      risk_reduction_pct = c(52, 52, 85, 35, 30)
    )
    
    ggplot(prevention_data, aes(x = reorder(strategy, risk_reduction_pct),
                                y = risk_reduction_pct,
                                fill = strategy)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#78c2ad", "#56cc9d", "#6cc3d5",
                                   "#ffce67", "#f3969a")) +
      geom_text(aes(label = paste0(risk_reduction_pct, "%")),
                hjust = -0.1, size = 4, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                         labels = function(x) paste0(x, "%")) +
      xlab("") +
      ylab("Estimated Risk Reduction") +
      theme_minimal() +
      theme(axis.text = element_text(size = 11),
            axis.title = element_text(size = 13))
  })
  #Hydration Calculator
  output$hydro_baseline <- renderText({ paste0(hydro_vals()$baseline, " oz") })
  output$hydro_extra    <- renderText({ paste0(hydro_vals()$extra, " oz") })
  output$hydro_total    <- renderText({ paste0(hydro_vals()$total, " oz") })
  
  output$hydro_tip <- renderText({
    total <- hydro_vals()$total
    if (total < 80)  "Spread your intake throughout the day — don't try to drink it all at once."
    else if (total < 120) "Drink ~20 oz before your workout, sip 6–8 oz every 15 min during, and rehydrate with 16–24 oz after."
    else "High activity level — consider electrolyte drinks to replace sodium and potassium lost through sweat."
  })
}