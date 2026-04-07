library(shiny)
library(plotly)
library(bslib)
library(leaflet)
library(markdown)
library(dplyr)
library(tidyr)

sport_injuries_by_age <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws(sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity)
  ) %>%
  group_by(sport_or_activity, year) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")
injuries_by_agegroup <- read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws(sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity)
  ) %>%
  group_by(sport_or_activity, year, X0_to_4, X4_to_15, X14_to_24, X25_to_64, X65_or_over, injuries) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")
fluidPage(
  theme = bs_theme(bootswatch = "minty") |>
    bs_add_variables("navbar-bg" = "#78c2ad"),
  h2("Sports Injury Reports", style = "text-align: center; font-size: 50px; margin-top: 10px; margin-bottom: 10px;"),
  tags$style(HTML("
  body {
    background-color: #f0f7f5;
  }
  .nav-tabs {
    background-color: #78c2ad;
    border-radius: 8px;
    padding: 4px;
  }
  .nav-tabs > li > a {
    color: white !important;
    font-weight: 600;
    border-radius: 6px !important;
  }
  .nav-tabs > li.active > a {
    background-color: white !important;
    color: #78c2ad !important;
  }
  .hero-banner {
    background: linear-gradient(135deg, #78c2ad 0%, #56cc9d 50%, #6cc3d5 100%);
    color: white;
    padding: 60px 40px;
    border-radius: 12px;
    margin-bottom: 30px;
    text-align: center;
    box-shadow: 0 4px 15px rgba(120,194,173,0.4);
  }
  .hero-banner h1 {
    font-size: 52px;
    font-weight: 800;
    margin-bottom: 10px;
    text-shadow: 1px 1px 3px rgba(0,0,0,0.15);
  }
  .hero-banner h4 {
    font-size: 18px;
    opacity: 0.9;
    font-weight: 400;
  }
  .nav-card {
    background: white;
    border-radius: 12px;
    padding: 25px;
    text-align: center;
    box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    transition: transform 0.2s, box-shadow 0.2s;
    border-top: 4px solid #78c2ad;
    height: 100%;
  }
  .nav-card:hover {
    transform: translateY(-4px);
    box-shadow: 0 6px 20px rgba(0,0,0,0.12);
  }
  .nav-card h4 {
    color: #2c3e50;
    font-weight: 700;
    margin-bottom: 10px;
  }
  .nav-card p {
    color: #666;
    font-size: 14px;
  }
  .about-section {
    background: white;
    border-radius: 12px;
    padding: 40px;
    margin-top: 30px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.06);
    border-left: 6px solid #78c2ad;
  }
  .well {
    background-color: #e8f5f1 !important;
    border: 1px solid #78c2ad !important;
    border-radius: 10px !important;
    box-shadow: 0 2px 8px rgba(120,194,173,0.2) !important;
  }
  .plot-box {
    background: white;
    border-radius: 10px;
    padding: 15px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.06);
    border: 1px solid #e0f0eb;
  }
  .tab-content {
    background-color: #f0f7f5;
    padding: 10px;
    border-radius: 8px;
  }
  .section-header {
    color: #78c2ad;
    font-weight: 700;
    border-bottom: 2px solid #78c2ad;
    padding-bottom: 8px;
    margin-bottom: 20px;
  }
  .irs-bar, .irs-bar-edge {
    background: #78c2ad !important;
    border-color: #78c2ad !important;
  }
  .irs-single {
    background: #78c2ad !important;
  }
  .btn-primary {
    background-color: #78c2ad !important;
    border-color: #78c2ad !important;
    border-radius: 20px !important;
    padding: 8px 20px !important;
    font-weight: 600 !important;
  }
  .btn-primary:hover {
    background-color: #56cc9d !important;
    border-color: #56cc9d !important;
  }
  .summary-card {
    border-radius: 12px !important;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1) !important;
    border: none !important;
  }
  
.nav-tabs .nav-link.active {
  background-color: #56cc9d !important;
  color: white !important;
}

.nav-tabs .nav-link {
  color: white !important;
}

.nav-tabs .nav-link:hover {
  background-color: #56cc9d !important;
  color: white !important;
}
")),
  
  
  
  tabsetPanel(id = "tabs",
              tabPanel("Home",
                       
                       div(class = "hero-banner", style = "margin-top: 20px;",
                           h1("Sports Injury Reports"),
                           div(style = "background: white; border-radius: 12px; padding: 25px 40px;
             box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin: 20px 10px;
             border-left: 6px solid #78c2ad; text-align: center;",
                               h4(style = "color: #78c2ad; font-weight: 700; margin-bottom: 15px;", "Did You Know?"),
                               uiOutput("fun_fact"),
                               br(),
                               actionButton("next_fact", "Next Fact →", class = "btn-primary")
                           ),
                           h4("Exploring Sports Injury Data from 2007 to 2024"),
                           br(),
                           div(style = "display: flex; justify-content: center; gap: 40px; flex-wrap: wrap; opacity: 0.9;",
                               div(style = "text-align: center;",
                                   h2(style = "font-size: 36px; font-weight: 800; margin: 0;", "18"),
                                   p(style = "margin: 0; font-size: 13px;", "Years of Data")
                               ),
                               div(style = "text-align: center;",
                                   h2(style = "font-size: 36px; font-weight: 800; margin: 0;", "50+"),
                                   p(style = "margin: 0; font-size: 13px;", "Sports Tracked")
                               ),
                               div(style = "text-align: center;",
                                   h2(style = "font-size: 36px; font-weight: 800; margin: 0;", "50"),
                                   p(style = "margin: 0; font-size: 13px;", "States Covered")
                               ),
                               div(style = "text-align: center;",
                                   h2(style = "font-size: 36px; font-weight: 800; margin: 0;", "5"),
                                   p(style = "margin: 0; font-size: 13px;", "Age Groups")
                               )
                           )
                       ),
                       
                       # Explore Section
                       h3("Explore the Data",
                          style = "text-align: center; font-weight: 700; color: #2c3e50;
              margin: 30px 0 5px 0; font-size: 32px;"),
                       p("Click any card below to dive into the data",
                         style = "text-align: center; color: #888; margin-bottom: 25px;"),
                       
                       fluidRow(style = "margin: 0 10px;",
                                column(4,
                                       div(class = "nav-card", style = "border-top-color: #78c2ad; margin-bottom: 20px;",
                                           div(style = "font-size: 40px; margin-bottom: 12px;"),
                                           h4("Sport Injuries Per Year"),
                                           p("See which sports cause the most injuries each year"),
                                           br(),
                                           actionButton("go_sport", "Explore →", class = "btn-primary")
                                       )

                                ),
                                column(4,
                                       div(class = "nav-card", style = "border-top-color: #f3969a; margin-bottom: 20px;",
                                           div(style = "font-size: 40px; margin-bottom: 12px;",),
                                           h4("Injuries by Age Group"),
                                           p("Explore how injuries vary across different age groups over time"),
                                           br(),
                                           actionButton("go_age", "Explore →", class = "btn-primary",
                                                        style = "background-color: #f3969a !important; border-color: #f3969a !important;")
                                       )
                                ),
                                column(4,
                                       div(class = "nav-card", style = "border-top-color: #6cc3d5; margin-bottom: 20px;",
                                           div(style = "font-size: 40px; margin-bottom: 12px;",),
                                           h4("Sport Injuries by Age"),
                                           p("Break down injuries by sport and age group with pie charts"),
                                           br(),
                                           actionButton("go_sport_age", "Explore →", class = "btn-primary",
                                                        style = "background-color: #6cc3d5 !important; border-color: #6cc3d5 !important;")
                                       )
                                )
                       ),
                       fluidRow(style = "margin: 0 10px;",
                                column(4,
                                       div(class = "nav-card", style = "border-top-color: #ffce67; margin-bottom: 20px;",
                                           div(style = "font-size: 40px; margin-bottom: 12px;"),
                                           h4("Favorite Sport by State"),
                                           p("Explore each state's favorite sport and corresponding injury facts"),
                                           br(),
                                           actionButton("go_state", "Explore →", class = "btn-primary",
                                                        style = "background-color: #ffce67 !important; border-color: #ffce67 !important;
                              color: #2c3e50 !important;")
                                       )
                                ),
                                column(4,
                                       div(class = "nav-card", style = "border-top-color: #56cc9d; margin-bottom: 20px;",
                                           div(style = "font-size: 40px; margin-bottom: 12px;"),
                                           h4("Injury Prevention"),
                                           p("Learn how to stay safe and reduce your risk of injury in your favorite sport"),
                                           br(),
                                           actionButton("go_prevention", "Explore →", class = "btn-primary",
                                                        style = "background-color: #56cc9d !important; border-color: #56cc9d !important;")
                                       )
                                ),
                                column(4,
                                       div(class = "nav-card", style = "border-top-color: #ff7851; margin-bottom: 20px;",
                                           div(style = "font-size: 40px; margin-bottom: 12px;"),
                                           h4("About Us"),
                                           p("Meet the team behind Sports Injury Reports"),
                                           br(),
                                           actionButton("go_about", "Meet the Team →", class = "btn-primary",
                                                        style = "background-color: #ff7851 !important; border-color: #ff7851 !important;")
                                       )
                                )
                       ),
                       br()
              ),
             
              tabPanel("Sport Injuries Per Year",
                       fluidRow(
                         br(),
                         div(
                           style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
                 padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                           h4(style = "color: #78c2ad; margin-top:0;", "Sport Injuries Per Year"),
                           p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                             "Use the slider on the left to filter by year, or press ▶ Play to animate injury trends from 2007 to 2024. Hover over any bar to see the exact sport and injury count. The cards below the slider highlight the most and least injured sport for the selected year.")
                         ),
                         column(12,
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 3,
                                    h4("Filter by Year"),
                                    sliderInput(inputId = "year",
                                                label = "Year",
                                                min = 2007,
                                                max = 2024,
                                                value = 2007,
                                                sep = "",
                                                animate = animationOptions(
                                                  interval = 2000,
                                                  loop = TRUE,
                                                  playButton = "▶ Play",
                                                  pauseButton = "⏸ Pause"
                                                )
                                    ),
                                    hr(),
                                    uiOutput("top_sport_boxes_wrapper")
                                  ),
                                  
                                  mainPanel(
                                    width = 9,
                                    div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; height: 950px; overflow: hidden; margin-bottom: 30px;",
                                        plotlyOutput("yearly_injuries_by_sport", height = "1000px")
                                    )
                                  )
                                ),
                         )
                       )
              ),
              
              
              tabPanel("Injuries By Age Group",
                       fluidRow(
                         br(),
                         div(
                           style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
               padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                           h4(style = "color: #78c2ad; margin-top:0;", "Injuries By Age Group"),
                           p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                             "Select an age group and one or more sports from the sidebar to compare injury counts across different age groups. Use the sport selector to add or remove sports from the chart. Below the chart, the Injury Trend Summary shows the top increasing and decreasing sports for the selected age group — use the numeric input to control how many sports appear in each list.")
                         ),
                         column(12,
                                hr(),
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 3,
                                    selectInput(inputId = "age_group",
                                                label = "Select Age Group",
                                                choices = c("0 to 4"     = "X0_to_4",
                                                            "4 to 15"    = "X4_to_15",
                                                            "14 to 24"   = "X14_to_24",
                                                            "25 to 64"   = "X25_to_64",
                                                            "65 or over" = "X65_or_over")
                                    ),
                                    selectInput(inputId = "sport_or_activity",
                                                label = "Select Sport(s)",
                                                choices  = unique(injuries_by_agegroup$sport_or_activity),
                                                selected = unique(injuries_by_agegroup$sport_or_activity)[1],
                                                multiple = TRUE),
                                    hr(),
                                    numericInput(inputId = "top_n_sports",
                                                 label = "Sports to show in trend summary",
                                                 value = 3,
                                                 min = 1,
                                                 max = 10,
                                                 step = 1)
                                  ),
                                  mainPanel(
                                    width = 9,
                                    div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 30px;",
                                        plotOutput("yearly_injuries_by_age")
                                    )
                                  )
                                )
                         ),
                         column(12,
                                br(),
                                h4(style = "color: #555; margin-bottom: 15px; padding-left: 15px;", "Injury Trend Summary"),
                                fluidRow(
                                  column(6,
                                         div(
                                           style = "background-color: #f8f9fa; border-left: 4px solid #f3969a;
               border-radius: 4px; padding: 15px; margin-bottom: 15px;",
                                           h5(style = "color: #f3969a; margin-top: 0;", "\u2191 Top Increasing Sports"),
                                           uiOutput("increasing_trends")
                                         )
                                  ),
                                  column(6,
                                         div(
                                           style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
               border-radius: 4px; padding: 15px; margin-bottom: 15px;",
                                           h5(style = "color: #78c2ad; margin-top: 0;", "\u2193 Top Decreasing Sports"),
                                           uiOutput("decreasing_trends")
                                         )
                                  )
                                ),
                         )
                       )
              ),
              tabPanel("Sport Injuries By Age",
                       fluidRow(
                         br(),
                         div(
                           style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
                 padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                           h4(style = "color: #78c2ad; margin-top:0;", "Sport Injuries By Age"),
                           p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                             "Select one or more sports from the sidebar to see a pie chart breakdown of injuries by age group for each sport. The legend on the left shows which color corresponds to each age group.")
                         ),
                         column(12,
                                hr(),
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 3,
                                    style = "position: sticky; top: 20px;",
                                    selectInput(inputId = "sport_or_activity",
                                                label = "Select Sport(s)",
                                                choices  = unique(injuries_by_agegroup$sport_or_activity),
                                                selected = unique(injuries_by_agegroup$sport_or_activity)[1],
                                                multiple = TRUE),
                                    hr(),
                                    p("Age Group Legend", style = "text-align: center; font-weight: bold; color: #555; margin-bottom: 0;"),
                                    plotOutput("age_legend", height = "180px"),
                                    hr(),
                                    p(style = "text-align: center; font-size: 13px;",
                                      tags$a(href = "#", 
                                             onclick = "document.querySelector('.nav-tabs a[data-value=\"Injuries By Age Group\"]').click(); return false;",
                                             style = "color: #f3969a; font-size: 15px;",
                                             "\u2192 View Injury Trends By Age Group")
                                    )
                                  ),
                                  mainPanel(
                                    width = 9,
                                    div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 30px;",
                                        uiOutput("sport_injuries_by_age")
                                    )
                                  )
                                )
                         )
                       )
              ),
             
              tabPanel("Favorite Sport By State",
                       br(),
                       div(
                         style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
                 padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                         h4(style = "color: #78c2ad; margin-top:0;", "Favorite Sport by State"),
                         div(
                           style = "display: flex; gap: 20px; align-items: center;
                   justify-content: center; margin-bottom: 15px;",
                           img(src = "https://upload.wikimedia.org/wikipedia/en/a/a2/National_Football_League_logo.svg", height = "50px"),
                           img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/05_NHL_Shield.svg", height = "50px"),
                           img(src = "https://upload.wikimedia.org/wikipedia/commons/d/dd/NCAA_logo.svg", height = "50px"),
                           img(src = "https://cdn.freebiesupply.com/logos/large/2x/nba-logo-png-transparent.png", height = "50px"),
                           img(src = "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg", height = "50px")
                         ),
                         p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                           "This map displays the most popular sport in each state based on fan interest and
           viewership data. Click on any circle to learn more about the most common injuries
           associated with that state's favorite sport. Hover over a circle to see the state
           name and sport at a glance.")
                       ),
                       leafletOutput("sport_map", height = "650px"),
                       
                       br(),
                       
                       # Summary cards
                       fluidRow(
                         column(4,
                                div(style = "background-color: #78c2ad; color: white; text-align: center;
                 border-radius: 10px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
                                    h4(style = "color: white; margin-top: 0;", "Most Popular Sport"),
                                    h2(style = "color: white;", textOutput("most_popular_sport"))
                                )
                         ),
                         column(4,
                                div(style = "background-color: #f3969a; color: white; text-align: center;
                 border-radius: 10px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
                                    h4(style = "color: white; margin-top: 0;", "Number of Sports"),
                                    h2(style = "color: white;", textOutput("num_sports"))
                                )
                         ),
                         column(4,
                                div(style = "background-color: #ffce67; color: white; text-align: center;
                 border-radius: 10px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
                                    h4(style = "color: white; margin-top: 0;", "States Tracked"),
                                    h2(style = "color: white;", textOutput("num_states"))
                                )
                         )
                       ),
                       
                       br(),
                       
                       # Bar chart
                       h4("States Per Sport", style = "text-align: center; color: #2c3e50;"),
                       plotOutput("sport_state_bar", height = "300px")
              ),

           
              
              
              navbarMenu("Injuries & Prevention",
                         tabPanel("General Information",
                                  fluidRow(
                                    column(12,
                                           h1("Injury Prevention",
                                              style = "text-align: center; margin-top: 40px; font-size: 40px;"),
                                           h4("Tips to keep you safe and active",
                                              style = "text-align: center; color: grey;"),
                                           hr()
                                    )
                                  ),
                                  fluidRow(
                                    column(4,
                                           div(style = "background-color: white; border-radius: 12px; padding: 25px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #78c2ad;
                   height: 100%; margin-bottom: 15px;",
                                               div(style = "text-align: center; font-size: 40px; margin-bottom: 10px;", "🏃"),
                                               h4(style = "color: #78c2ad; font-weight: 700; text-align: center;", "General Tips"),
                                               tags$ul(style = "color: #555; font-size: 20px; line-height: 1.8;",
                                                       tags$li("Always warm up before activity and cool down afterward"),
                                                       tags$li("Stay hydrated before, during, and after exercise"),
                                                       tags$li("Know your limits and gradually increase intensity"),
                                                       tags$li("Get adequate rest and recovery between sessions")
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(style = "background-color: white; border-radius: 12px; padding: 25px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #f3969a;
                   height: 100%; margin-bottom: 15px;",
                                               div(style = "text-align: center; font-size: 40px; margin-bottom: 10px;", "🏈"),
                                               h4(style = "color: #f3969a; font-weight: 700; text-align: center;", "Equipment"),
                                               tags$ul(style = "color: #555; font-size: 20px; line-height: 1.8;",
                                                       tags$li("Wear appropriate protective gear for your sport"),
                                                       tags$li("Make sure equipment fits properly"),
                                                       tags$li("Replace worn out shoes and gear regularly")
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(style = "background-color: white; border-radius: 12px; padding: 25px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #6cc3d5;
                   height: 100%; margin-bottom: 15px;",
                                               div(style = "text-align: center; font-size: 40px; margin-bottom: 10px;", "🎯"),
                                               h4(style = "color: #6cc3d5; font-weight: 700; text-align: center;", "Technique"),
                                               tags$ul(style = "color: #555; font-size: 20px; line-height: 1.8;",
                                                       tags$li("Learn proper form from a coach or trainer"),
                                                       tags$li("Avoid overuse by varying your activities"),
                                                       tags$li("Listen to your body and stop if something hurts")
                                               )
                                           )
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    column(8, offset = 2,
                                           div(style = "background: linear-gradient(135deg, #78c2ad 0%, #6cc3d5 100%);
                 border-radius: 12px; padding: 30px; text-align: center;
                 box-shadow: 0 4px 15px rgba(120,194,173,0.4);",
                                               h4(style = "color: white; font-weight: 700; font-size: 25px;",
                                                  "Remember: Prevention is always better than treatment!"),
                                               p(style = "color: white; opacity: 0.9; font-size: 20px; margin-bottom: 20px;",
                                                 "Taking small steps before and after activity can save you weeks of recovery time."),
                                               tags$style(HTML("
        .topic-btn {
          background-color: white;
          color: #78c2ad;
          border: none;
          padding: 8px 16px;
          border-radius: 20px;
          font-size: 14px;
          font-weight: bold;
          cursor: pointer;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .topic-btn:hover {
          transform: translateY(-4px);
          box-shadow: 0 6px 16px rgba(0,0,0,0.2);
        }
      ")),
                                               hr(),
                                               p(style = "color: white; font-size: 16px; font-weight: bold; margin-top: 15px; margin-bottom: 10px;",
                                                 "Learn more about specific injuries and ways to prevent:"),
                                               div(
                                                 style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
                                                 tags$button("Nutrition & Recovery", class = "topic-btn",
                                                             onclick = "document.querySelector('.nav-tabs a[data-value=\"Nutrition & Recovery\"]').click();"),
                                                 tags$button("Concussion", class = "topic-btn",
                                                             onclick = "document.querySelector('.nav-tabs a[data-value=\"Concussion\"]').click();"),
                                                 tags$button("ACL Injuries", class = "topic-btn",
                                                             onclick = "document.querySelector('.nav-tabs a[data-value=\"ACL Injuries\"]').click();"),
                                                 tags$button("Ankle Sprain", class = "topic-btn",
                                                             onclick = "document.querySelector('.nav-tabs a[data-value=\"Ankle Sprain\"]').click();")
                                               )
                                           )
                                    )
                                  ),
                                  br(),
                                  h3("Resources", style = "text-align: center; color: #2c3e50; font-weight: 700;"),
                                  hr(style = "width: 100px; border-top: 2px solid #78c2ad; margin: 10px auto 20px;"),
                                  fluidRow(
                                    column(4,
                                           div(style = "background-color: white; border-radius: 12px; padding: 25px;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #78c2ad;
                 height: 100%; margin-bottom: 15px;",
                                               div(style = "text-align: center; font-size: 35px; margin-bottom: 10px;"),
                                               h4(style = "color: #78c2ad; font-weight: 700; text-align: center;", "For Athletes"),
                                               tags$ul(style = "color: #555; font-size: 14px; line-height: 2;",
                                                       tags$li(tags$a(href = "https://www.stopsportsinjuries.org", target = "_blank",
                                                                      "STOP Sports Injuries")),
                                                       tags$li(tags$a(href = "https://www.aossm.org", target = "_blank",
                                                                      "American Orthopaedic Society for Sports Medicine")),
                                                       tags$li(tags$a(href = "https://www.nata.org", target = "_blank",
                                                                      "National Athletic Trainers Association"))
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(style = "background-color: white; border-radius: 12px; padding: 25px;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #f3969a;
                 height: 100%; margin-bottom: 15px;",
                                               div(style = "text-align: center; font-size: 35px; margin-bottom: 10px;"),
                                               h4(style = "color: #f3969a; font-weight: 700; text-align: center;", "For Parents"),
                                               tags$ul(style = "color: #555; font-size: 14px; line-height: 2;",
                                                       tags$li(tags$a(href = "https://www.healthychildren.org/English/healthy-living/sports/Pages/default.aspx",
                                                                      target = "_blank", "AAP Healthy Children - Sports")),
                                                       tags$li(tags$a(href = "https://www.cdc.gov/headsup/index.html", target = "_blank",
                                                                      "CDC Heads Up - Concussion Safety")),
                                                       tags$li(tags$a(href = "https://www.safekids.org/safetytips", target = "_blank",
                                                                      "Safe Kids Worldwide"))
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(style = "background-color: white; border-radius: 12px; padding: 25px;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #6cc3d5;
                 height: 100%; margin-bottom: 15px;",
                                               div(style = "text-align: center; font-size: 35px; margin-bottom: 10px;"),
                                               h4(style = "color: #6cc3d5; font-weight: 700; text-align: center;", "For Coaches"),
                                               tags$ul(style = "color: #555; font-size: 14px; line-height: 2;",
                                                       tags$li(tags$a(href = "https://www.ncaa.org/news/2023/9/15/media-center-sport-science-institute-distributes-recommendations-to-prevent-catastrophic-injury-and-death-in-college-sports",
                                                                      target = "_blank", "NCAA Sport Science Institute")),
                                                       tags$li(tags$a(href = "https://www.nsca.com", target = "_blank",
                                                                      "National Strength and Conditioning Association")),
                                                       tags$li(tags$a(href = "https://www.usada.org", target = "_blank",
                                                                      "U.S. Anti-Doping Agency - Clean Sport"))
                                               )
                                           )
                                    )
                                  ),
                                  br()
                         ),
                         
                         tabPanel("Nutrition & Recovery",
                                  fluidRow(
                                    column(12,
                                           div(
                                             style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
                                  padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                                             h4(style = "color: #78c2ad; margin-top:0;", "Nutrition & Recovery"),
                                             p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                                               "Key nutritional strategies for injury prevention and recovery in athletes.")
                                           ),
                                           
                                           tags$style(HTML("
                                  .nutrition-details {
                                  border-radius: 10px;
                                  padding: 14px 18px;
                                  margin-bottom: 12px;
                                  border-left: 6px solid #78c2ad;
                                  background-color: #f9fefc;
                                  box-shadow: 0 2px 8px rgba(0,0,0,0.05);
                                  transition: box-shadow 0.2s;
                                    }
                                  .nutrition-details[open] {
                                  box-shadow: 0 4px 14px rgba(0,0,0,0.10);
                                    }
        .nutrition-details summary {
          font-size: 18px;
          font-weight: 600;
          cursor: pointer;
          list-style: none;
          display: flex;
          align-items: center;
          gap: 10px;
        }
        .nutrition-details summary::-webkit-details-marker { display: none; }
        .nutrition-details summary::before {
          content: '▶';
          font-size: 11px;
          transition: transform 0.2s;
        }
        .nutrition-details[open] summary::before {
          transform: rotate(90deg);
        }
        .nutrition-details p {
          margin-top: 10px;
          font-size: 15px;
          color: #444;
        }
        .nutr-teal   { border-left-color: #78c2ad; background-color: #f0faf7; }
        .nutr-teal   summary { color: #78c2ad; }
        .nutr-coral  { border-left-color: #f3969a; background-color: #fff5f6; }
        .nutr-coral  summary { color: #f3969a; }
        .nutr-blue   { border-left-color: #6cc3d5; background-color: #f0f9fc; }
        .nutr-blue   summary { color: #6cc3d5; }
        .nutr-yellow { border-left-color: #ffce67; background-color: #fffdf0; }
        .nutr-yellow summary { color: #d4a800; }
        .nutr-green  { border-left-color: #56cc9d; background-color: #f0fdf7; }
        .nutr-green  summary { color: #56cc9d; }
        .nutr-orange { border-left-color: #ff7851; background-color: #fff5f2; }
        .nutr-orange summary { color: #ff7851; }
      ")),
                                           
                                           
                                           hr(),
                                           div(style = "background:#e8f5f1; border-radius:12px; padding:20px; border: 1px solid #78c2ad; margin-bottom:15px;",
                                               h4("Hydration Calculator", style = "color:#555; font-weight:700; margin-bottom:15px;"),
                                               fluidRow(
                                                 column(4,
                                                        sliderInput("hydro_weight", "Body weight (lbs)", min = 80, max = 300, value = 160, step = 1)
                                                 ),
                                                 column(4,
                                                        sliderInput("hydro_duration", "Activity duration (min)", min = 15, max = 180, value = 60, step = 15)
                                                 ),
                                                 column(4,
                                                        selectInput("hydro_intensity", "Intensity",
                                                                    choices = c("Low (walking, yoga)" = "1",
                                                                                "Moderate (running, cycling)" = "1.3",
                                                                                "High (HIIT, team sport)" = "1.6"),
                                                                    selected = "1.3")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(4, div(style = "background:#e8f4fb; border-radius:10px; padding:15px; text-align:center; border-top: 4px solid #6cc3d5;",
                                                               p("Daily baseline", style = "color:#555; margin:0; font-size:13px;"),
                                                               h3(textOutput("hydro_baseline"), style = "color:#6cc3d5; margin:5px 0 0;"),
                                                               p("oz / day", style = "color:#888; margin:0; font-size:12px;")
                                                 )),
                                                 column(4, div(style = "background:#fdf0f1; border-radius:10px; padding:15px; text-align:center; border-top: 4px solid #f3969a;",
                                                               p("Exercise addition", style = "color:#555; margin:0; font-size:13px;"),
                                                               h3(textOutput("hydro_extra"), style = "color:#f3969a; margin:5px 0 0;"),
                                                               p("oz", style = "color:#888; margin:0; font-size:12px;")
                                                 )),
                                                 column(4, div(style = "background:#56cc9d; border-radius:10px; padding:15px; text-align:center; border-top: 4px solid #3aaa7a;",
                                                               p("Total target", style = "color:white; margin:0; font-size:13px;"),
                                                               h3(textOutput("hydro_total"), style = "color:white; margin:5px 0 0;"),
                                                               p("oz / day", style = "color:white; margin:0; font-size:12px;")
                                                 ))
                                               ),
                                               br(),
                                               br(),
                                               div(style = "text-align:center;",
                                                   uiOutput("hydro_bottles")
                                               ),
                                               hr(),
                                               div(style = "background:#e8f5f1; border-radius:12px; padding:20px; border: 1px solid #78c2ad; margin-bottom:15px;",
                                                   h4("Nutrient Timing", style = "color:#555; font-weight:700; margin-bottom:15px;"),
                                                   selectInput("timing_sport", "Sport type",
                                                               choices = c("Endurance (running, cycling)" = "endurance",
                                                                           "Strength (weightlifting)"      = "strength",
                                                                           "Team sport (soccer, basketball)" = "team")
                                                   ),
                                                   fluidRow(
                                                     column(4, div(style = "background:#e8f4fb; border-top:4px solid #6cc3d5; border-radius:10px; padding:15px;",
                                                                   h5("Pre-workout", style = "color:#6cc3d5; font-weight:700; margin-top:0;"),
                                                                   textOutput("timing_pre_time"),
                                                                   br(),
                                                                   uiOutput("timing_pre_foods")
                                                     )),
                                                     column(4, div(style = "background:#fdf0f1; border-top:4px solid #f3969a; border-radius:10px; padding:15px;",
                                                                   h5("During", style = "color:#f3969a; font-weight:700; margin-top:0;"),
                                                                   textOutput("timing_during_time"),
                                                                   br(),
                                                                   uiOutput("timing_during_foods")
                                                     )),
                                                     column(4, div(style = "background:#f0fdf7; border-top:4px solid #56cc9d; border-radius:10px; padding:15px;",
                                                                   h5("Post-workout", style = "color:#56cc9d; font-weight:700; margin-top:0;"),
                                                                   textOutput("timing_post_time"),
                                                                   br(),
                                                                   uiOutput("timing_post_foods")
                                                     ))
                                                   )
                                               ),
                                           ),
                                           
                                           hr(),
                                           h4("Nutrient Details", style = "color:#555; font-weight:700; margin-bottom:15px;"),
                                           tags$details(class = "nutrition-details nutr-teal",
                                                        tags$summary("🍞  Carbohydrates"),
                                                        p(strong("Role: "), "Prevents muscle and mental fatigue; supports energy for sustained performance and recovery."),
                                                        p(strong("Examples: "), "Consume as part of balanced meals, especially around training sessions — rice, oats, pasta, fruit.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-coral",
                                                        tags$summary("🥩  Proteins"),
                                                        p(strong("Role: "), "Essential for muscle repair and adaptation, reducing the risk of overuse injuries."),
                                                        p(strong("Examples: "), "Ingest 20–30 grams post-training — chicken, eggs, Greek yogurt, legumes.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-blue",
                                                        tags$summary("🐟  Healthy Fats (Omega-3 Fatty Acids)"),
                                                        p(strong("Role: "), "Supports joint health and reduces chronic inflammation linked to repetitive stress."),
                                                        p(strong("Examples: "), "Include foods like salmon, walnuts, and flaxseeds regularly in the diet.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-yellow",
                                                        tags$summary("☀️  Calcium and Vitamin D"),
                                                        p(strong("Role: "), "Strengthens bones and reduces the risk of stress fractures."),
                                                        p(strong("Examples: "), "Supplement with 800 IU/day of vitamin D and 2,000 mg of calcium — dairy, fortified milk, sunlight.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-green",
                                                        tags$summary("🫐  Antioxidants"),
                                                        p(strong("Role: "), "Protects muscles from oxidative stress, speeding up recovery and reducing soreness."),
                                                        p(strong("Examples: "), "Incorporate berries, citrus fruits, and leafy greens.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-blue",
                                                        tags$summary("💧  Hydration"),
                                                        p(strong("Role: "), "Maintains joint lubrication, muscle elasticity, and optimal coordination."),
                                                        p(strong("Examples: "), "Ensure consistent hydration before, during, and after high-impact activities — water, electrolyte drinks.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-orange",
                                                        tags$summary("🌿  Anti-inflammatory Nutrients"),
                                                        p(strong("Role: "), "Reduces chronic inflammation and supports tissue resilience against repetitive strain."),
                                                        p(strong("Examples: "), "Use turmeric, ginger, and omega-3 supplements.")
                                           ),
                                           
                                           tags$details(class = "nutrition-details nutr-teal",
                                                        tags$summary("⏱️  Timing of Nutrient Intake"),
                                                        p(strong("Role: "), "Enhances recovery by promoting muscle repair and glycogen replenishment post-training."),
                                                        p(strong("Examples: "), "Consume protein and carbohydrates within 30 minutes post-exercise.")
                                           ),
                                           br()
                                    )
                                  )
                         ),
                         tabPanel("Concussion",
                                  fluidRow(
                                    br(),
                                    div(
                                      style = "background-color: #f8f9fa; border-left: 4px solid #f3969a;
               padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                                      h4(style = "color: #f3969a; margin-top:0;", "Concussion"),
                                      p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                                        "A concussion is a mild traumatic brain injury causing short-term effects such as headaches, memory issues, and balance problems. 
        They are most commonly caused by falls or impacts during contact sports, 
        and while some may result in loss of consciousness, most people recover fully (Mayo Clinic).",
                                        br(), br(),
                                        "The chart below shows the number of concussions recorded across 13 collegiate sports over four academic years (2011–2015). 
        Each bar represents a sport, with colors indicating the individual year's contribution to the total count."
                                      )
                                    ),
                                    
                                    column(8, offset = 2,
                                           div(
                                             style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 30px;",
                                             plotOutput("concussion_plot", height = "500px")
                                           )
                                    ),
                                    
                                    h4(style = "color: #555; margin-bottom: 15px; padding-left: 15px;", "Concussion Prevention"),
                                    
                                    # Row 1: Rule Changes & Protective Equipment
                                    fluidRow(
                                      column(6,
                                             div(
                                               style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
                   border-radius: 4px; padding: 15px; min-height: 180px; margin-bottom: 15px;",
                                               h5(style = "color: #78c2ad; margin-top: 0; font-size: 20px;", "Rule Changes & Education"),
                                               p(style = "color: #2c3e50; font-size: 18px; margin: 0;",
                                                 "Enforcing safe play rules, teaching proper tackling and heading techniques, 
            and increasing athlete and coach awareness remain foundational prevention strategies."
                                               )
                                             )
                                      ),
                                      column(6,
                                             div(
                                               style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
                   border-radius: 4px; padding: 15px; min-height: 180px; margin-bottom: 15px;",
                                               h5(style = "color: #78c2ad; margin-top: 0; font-size: 20px;", "Protective Equipment"),
                                               p(style = "color: #2c3e50; font-size: 18px; margin: 0;",
                                                 "Helmets prevent skull fractures but have not been shown to significantly 
            reduce concussion risk, as they cannot prevent the brain from moving 
            inside the skull during impact."
                                               )
                                             )
                                      )
                                    ),
                                    
                                    # Row 2: Q-Collar full width
                                    fluidRow(
                                      column(12,
                                             div(
                                               style = "background-color: #fde8ec; border-left: 4px solid #f3969a;
                   border-radius: 4px; padding: 15px; margin-bottom: 15px;",
                                               h5(style = "color: #f3969a; margin-top: 0; font-size: 20px;",
                                                  "\U0001f9e0 The Q-Collar",
                                                  
                                                  tags$span(
                                                    style = "display: inline-block; background-color: #f3969a; color: white;
                       font-size: 10px; font-weight: bold; padding: 3px 8px;
                       border-radius: 20px; text-transform: uppercase; letter-spacing: 0.5px;",
                                                    "New Technology"
                                                  ),
                                                  br(),
                                                  tags$a(
                                                    href = "https://www.qcollar.com",
                                                    target = "_blank",
                                                    style = "display: inline-block; background-color: #f3969a; color: white;
                     padding: 8px 16px; border-radius: 20px; text-decoration: none;
                     font-size: 14px; font-weight: bold; margin-top: 8px;",
                                                    "\U0001f517 Visit Q-Collar Website"
                                                  ),
                                               ),
                                               p(style = "color: #2c3e50; font-size: 18px; margin: 0 0 15px 0;",
                                                 "A neck collar that compresses the jugular veins to increase cerebral blood volume,
            acting like an ", strong("\"airbag for the brain\""),
                                                 " to reduce movement during impact. ",
                                                 em("Myer et al. (2016, Br J Sports Med)"),
                                                 " studied 62 high school varsity football players across a full season — here's what they found:"
                                               ),
                                               
                                               # Stat boxes
                                               
                                               fluidRow(
                                                 column(4, offset = 2,
                                                        div(
                                                          style = "background-color: #fff; border: 2px solid #f3969a; border-radius: 6px; padding: 20px;
               text-align: center; margin-bottom: 12px;",
                                                          div(style = "font-size: 36px; font-weight: bold; color: #2c3e50; line-height: 1;",
                                                              "500K"),
                                                          div(style = "font-size: 14px; color: #666; margin-top: 10px;",
                                                              "impacts measured across the study")
                                                        )
                                                 ),
                                                 column(4,
                                                        div(
                                                          style = "background-color: #f3969a; border-radius: 6px; padding: 20px;
               text-align: center; margin-bottom: 8px;",
                                                          div(
                                                            style = "display: flex; align-items: center; justify-content: center; gap: 8px;",
                                                            tags$span(style = "font-size: 36px; font-weight: bold; color: white; line-height: 1;",
                                                                      "\u2193"),
                                                            tags$span(style = "font-size: 36px; font-weight: bold; color: white; line-height: 1;",
                                                                      "66%")
                                                          ),
                                                          div(style = "font-size: 14px; color: white; opacity: 0.9; margin-top: 10px;",
                                                              "in the likelihood of brain damage")
                                                        )
                                                 )
                                               )
                                               
                                               
                                             )
                                      )
                                    ),
                                    br()
                                  )
                         ),
                         tabPanel("ACL Injuries",
                                  br(),
                                  h1("ACL Injuries in Sport", style = "text-align: center; font-size: 40px;"),
                                  h4("Who is at risk and how to prevent",
                                     style = "text-align: center; color: grey;"),
                                  hr(),
                                  fluidRow(
                                    column(8, offset = 2,
                                           div(style = "background: white; border-radius: 12px; padding: 20px;
             box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin-bottom: 20px;",
                                               h4(style = "color: #56cc9d; font-weight: 700; text-align: center;",
                                                  "ACL Injury Prevention: Estimated Risk Reduction by Strategy"),
                                               p(style = "text-align: center; color: grey; font-size: 13px; margin-bottom: 15px;",
                                                 "Evidence-based programs combining neuromuscular training and strength work
                show up to 52% risk reduction in female athletes and 85% in male athletes."),
                                               plotOutput("acl_prevention", height = "300px"),
                                               p(style = "text-align: center; color: grey; font-size: 12px; margin-top: 12px;",
                                                 HTML("Source: Nessler et al. (2017). <em>ACL Injury Prevention: What Does Research Tell Us?</em>
                     Curr Rev Musculoskelet Med. &nbsp;
                     <a href='https://pmc.ncbi.nlm.nih.gov/articles/PMC5577417/' target='_blank'>
                     PMC5577417</a>"))
                                           )
                                    )
                                  ), 
                                  # Row 1 - Total reconstructions + gender split
                                  fluidRow(
                                    column(6,
                                           div(style = "background: white; border-radius: 12px; padding: 20px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin-bottom: 20px;",
                                               h4(style = "color: #78c2ad; font-weight: 700; text-align: center;",
                                                  "Total ACL Reconstructions by Sport"),
                                               plotOutput("acl_total_bar", height = "300px")
                                           )
                                    ),
                                    column(6,
                                           div(style = "background: white; border-radius: 12px; padding: 20px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin-bottom: 20px;",
                                               h4(style = "color: #f3969a; font-weight: 700; text-align: center;",
                                                  "Male vs Female Split by Sport"),
                                               plotOutput("acl_gender_split", height = "300px")
                                           )
                                    )
                                  ),
                                  
                                  # Row 2 - Risk % by gender + notch width
                                  fluidRow(
                                    column(6,
                                           div(style = "background: white; border-radius: 12px; padding: 20px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin-bottom: 20px;",
                                               h4(style = "color: #6cc3d5; font-weight: 700; text-align: center;",
                                                  "ACL Risk % by Sport and Gender"),
                                               plotOutput("acl_risk_gender", height = "300px")
                                           )
                                    ),
                                    column(6,
                                           div(style = "background: white; border-radius: 12px; padding: 20px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin-bottom: 20px;",
                                               h4(style = "color: #ffce67; font-weight: 700; text-align: center;",
                                                  "Intercondylar Notch Width: Normal vs ACL Tear"), p(style = "text-align: center; color: grey; font-size: 13px; margin-bottom: 15px;",
                                                                                                      "The intercondylar notch is the groove at the base of the femur (thigh bone) 
   through which the ACL passes. A narrower notch means less space for the ACL 
   during rotation, increasing tear risk — females tend to have narrower notch 
   widths on average, contributing to their higher injury rates."),
                                               plotOutput("acl_notch_width", height = "300px")
                                           )
                                    )
                                  ),
                      
                             
                                  
                                  # Research callout banner
                                  fluidRow(
                                    column(8, offset = 2,
                                           div(style = "background: linear-gradient(135deg, #78c2ad 0%, #6cc3d5 100%);
                   border-radius: 12px; padding: 30px; text-align: center;
                   box-shadow: 0 4px 15px rgba(120,194,173,0.4); margin-bottom: 20px;",
                                               h4(style = "color: white; font-weight: 700; font-size: 18px;",
                                                  "Female athletes are approximately twice as likely to tear their ACL as male athletes."),
                                               p(style = "color: white; opacity: 0.9; font-size: 14px; margin: 0;",
                                                 "Source: Shelbourne Knee Center, internal research report 2018")
                                           )
                                    )
                                  ),
                                  br()
                         ),
                         tabPanel("Ankle Sprain",
                                  fluidRow(
                                    br(),
                                    div(
                                      style = "background-color: #f8f9fa; border-left: 4px solid #78c2ad;
               padding: 15px; margin-bottom: 15px; border-radius: 4px;",
                                      h4(style = "color: #78c2ad; margin-top:0;", "Ankle Sprains"),
                                      p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                                        "An ankle sprain occurs when the ligaments supporting the ankle are stretched
         or torn, typically from rolling, twisting, or turning the ankle awkwardly.
         They are one of the most common injuries in athletics, often happening during
         landing, cutting, or contact with another player."),
                                      br(),
                                      p(style = "margin: 0; color: #2c3e50; font-size: 20px;",
                                        "The chart below shows lateral ankle sprain counts across collegiate sports,
         broken down by whether the injury occurred in practice or competition from 2009 to 2015.")
                                    ),
                                    column(8, offset = 2,
                                           div(
                                             style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 30px;",
                                             plotOutput("ankle_sprain_plot", height = "500px")
                                           )
                                    ),
                                    
                                    column(10, offset = 1,
                                           br(),
                                           tags$style(HTML("
        .ankle-tip-card {
          background: #ffffff;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 0;
          margin-bottom: 10px;
          cursor: pointer;
          transition: border-color 0.15s;
          overflow: hidden;
        }
        .ankle-tip-card:hover { border-color: #78c2ad; }
        .ankle-tip-header {
          display: flex;
          align-items: center;
          gap: 12px;
          padding: 14px 16px;
        }
        .ankle-tip-title { font-size: 15px; font-weight: 500; color: #2c3e50; margin: 0; }
        .ankle-tip-subtitle { font-size: 13px; color: #6c757d; margin: 2px 0 0; }
        .ankle-tip-chevron { margin-left: auto; font-size: 11px; color: #aaa; transition: transform 0.2s; }
        .ankle-tip-body {
          max-height: 0; overflow: hidden;
          transition: max-height 0.3s ease;
          background: #f8f9fa;
        }
        .ankle-tip-body.open { max-height: 400px; }
        .ankle-tip-body-inner {
          padding: 12px 16px 14px 20px;
          font-size: 14px; color: #495057; line-height: 1.7;
        }
        .ankle-tip-body-inner ul { margin: 0; padding-left: 16px; }
        .ankle-tip-body-inner li { margin-bottom: 4px; }
        .ankle-badge {
          display: inline-block; font-size: 11px; font-weight: 500;
          padding: 2px 8px; border-radius: 20px; margin-left: 8px; vertical-align: middle;
        }
        .ankle-badge-green { background: #e1f5ee; color: #085041; }
        .ankle-badge-amber { background: #faeeda; color: #633806; }

        .ankle-q-card {
          background: #ffffff;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 16px; margin-bottom: 10px;
        }
        .ankle-q-text { font-size: 15px; font-weight: 500; color: #2c3e50; margin: 0 0 12px; }
        .ankle-opt-btn {
          display: block; width: 100%; text-align: left;
          background: #ffffff; border: 1px solid #ddd;
          border-radius: 6px; padding: 10px 14px;
          font-size: 14px; color: #2c3e50; cursor: pointer;
          margin-bottom: 6px; transition: background 0.12s, border-color 0.12s;
          font-family: inherit;
        }
        .ankle-opt-btn:hover { background: #f8f9fa; border-color: #78c2ad; }
        .ankle-opt-btn.selected-good { border-color: #78c2ad; background: #e1f5ee; color: #085041; }
        .ankle-opt-btn.selected-bad  { border-color: #e24b4a; background: #fcebeb; color: #791f1f; }
        .ankle-opt-btn.revealed-good { border-color: #78c2ad; background: #e1f5ee; color: #085041; }
        .ankle-opt-btn:disabled { cursor: default; opacity: 0.85; }
        .ankle-feedback {
          font-size: 13px; margin-top: 8px; padding: 8px 12px;
          border-radius: 6px; line-height: 1.5;
        }
        .ankle-feedback-good { background: #e1f5ee; color: #085041; }
        .ankle-feedback-bad  { background: #fcebeb; color: #791f1f; }
        .ankle-step-dots { display: flex; gap: 6px; margin-bottom: 16px; }
        .ankle-dot {
          width: 8px; height: 8px; border-radius: 50%;
          background: #f8f9fa; border: 1px solid #bbb;
          transition: background 0.2s;
        }
        .ankle-dot.done   { background: #78c2ad; border-color: #78c2ad; }
        .ankle-dot.active { background: #495057; border-color: #495057; }
        .ankle-nav-row { display: flex; justify-content: space-between; align-items: center; margin-top: 12px; }
        .ankle-nav-btn {
          background: #ffffff; border: 1px solid #bbb;
          border-radius: 6px; padding: 8px 16px;
          font-size: 14px; color: #2c3e50; cursor: pointer; font-family: inherit;
          transition: background 0.12s;
        }
        .ankle-nav-btn:hover { background: #f8f9fa; border-color: #78c2ad; }
        .ankle-result-card {
          background: #ffffff; border: 1px solid #ddd;
          border-radius: 8px; padding: 24px; text-align: center;
        }
        .ankle-risk-badge {
          display: inline-block; font-size: 13px; font-weight: 500;
          padding: 4px 14px; border-radius: 20px; margin-bottom: 12px;
        }
        .ankle-risk-low  { background: #e1f5ee; color: #085041; }
        .ankle-risk-mod  { background: #faeeda; color: #633806; }
        .ankle-risk-high { background: #fcebeb; color: #791f1f; }
        .ankle-result-tips { text-align: left; margin-top: 14px; font-size: 14px; color: #495057; line-height: 1.7; }
        .ankle-result-tips li { margin-bottom: 4px; }
        .ankle-section-title {
          font-size: 18px; font-weight: 500; color: #2c3e50; margin: 0 0 4px;
        }
        .ankle-section-sub { font-size: 13px; color: #6c757d; margin: 0 0 16px; }
      .ankle-opt-btn.selected { border-color: #495057; background: #f0f0f0; color: #2c3e50; }
.ankle-feedback-neutral { background: #f0f4f8; color: #2c3e50; }
                                                           ")),
                                           
                                           # ── Tips ──────────────────────────────────────
                                           div(style = "margin-bottom: 8px;",
                                               p(class = "ankle-section-title", "Ankle sprain prevention"),
                                               p(class = "ankle-section-sub", "Tap any tip to learn more")
                                           ),
                                           div(id = "ankle-tips-list",
                                               
                                               # Tip 1
                                               div(class = "ankle-tip-card", onclick = "ankleTip(this)",
                                                   div(class = "ankle-tip-header",
                                                       div(
                                                         p(class = "ankle-tip-title", HTML('Wear the right shoes <span class="ankle-badge ankle-badge-amber">key factor</span>')),
                                                         p(class = "ankle-tip-subtitle", "Sport-specific footwear matters")
                                                       ),
                                                       span(class = "ankle-tip-chevron", "▼")
                                                   ),
                                                   div(class = "ankle-tip-body",
                                                       div(class = "ankle-tip-body-inner",
                                                           tags$ul(
                                                             tags$li("Choose shoes designed for your specific sport — running shoes for forward motion, cross-trainers for lateral movement"),
                                                             tags$li("Look for good arch support, a stable heel base, and cushioning under the ball of the foot"),
                                                             tags$li("Replace shoes regularly — worn-out soles lose the support that protects your ankles"),
                                                             tags$li("Try shoes on at the end of the day when feet are slightly swollen for the most accurate fit")
                                                           )
                                                       )
                                                   )
                                               ),
                                               
                                               # Tip 2
                                               div(class = "ankle-tip-card", onclick = "ankleTip(this)",
                                                   div(class = "ankle-tip-header",
                                                       div(
                                                         p(class = "ankle-tip-title", HTML('Warm up before activity <span class="ankle-badge ankle-badge-green">essential</span>')),
                                                         p(class = "ankle-tip-subtitle", "5–10 min before every session")
                                                       ),
                                                       span(class = "ankle-tip-chevron", "▼")
                                                   ),
                                                   div(class = "ankle-tip-body",
                                                       div(class = "ankle-tip-body-inner",
                                                           tags$ul(
                                                             tags$li("Always warm up for at least 5–10 minutes before any physical activity"),
                                                             tags$li("Include ankle circles, leg swings, and light jogging to increase blood flow"),
                                                             tags$li("Use dynamic stretches that mimic the movements of your sport"),
                                                             tags$li("Start at low intensity and gradually build up to your full workout pace")
                                                           )
                                                       )
                                                   )
                                               ),
                                               
                                               # Tip 3
                                               div(class = "ankle-tip-card", onclick = "ankleTip(this)",
                                                   div(class = "ankle-tip-header",
                                                       div(
                                                         p(class = "ankle-tip-title", HTML('Strengthen your ankle muscles <span class="ankle-badge ankle-badge-green">essential</span>')),
                                                         p(class = "ankle-tip-subtitle", "Calf raises & resistance bands")
                                                       ),
                                                       span(class = "ankle-tip-chevron", "▼")
                                                   ),
                                                   div(class = "ankle-tip-body",
                                                       div(class = "ankle-tip-body-inner",
                                                           tags$ul(
                                                             tags$li("Do heel raises daily — rise onto the balls of your feet and slowly lower back down, 10 reps"),
                                                             tags$li("Use a resistance band for ankle eversion — trains the peroneal muscles that act as a natural brace"),
                                                             tags$li("Gradually condition your leg and ankle muscles — don't rush into high-intensity activity"),
                                                             tags$li("Repeat exercises 10–15 times, 3 sets per day")
                                                           )
                                                       )
                                                   )
                                               ),
                                               
                                               # Tip 4
                                               div(class = "ankle-tip-card", onclick = "ankleTip(this)",
                                                   div(class = "ankle-tip-header",
                                                       div(
                                                         p(class = "ankle-tip-title", "Train your balance & proprioception"),
                                                         p(class = "ankle-tip-subtitle", "Your ankle's \"sixth sense\"")
                                                       ),
                                                       span(class = "ankle-tip-chevron", "▼")
                                                   ),
                                                   div(class = "ankle-tip-body",
                                                       div(class = "ankle-tip-body-inner",
                                                           tags$ul(
                                                             tags$li("Balance on one foot for 30 seconds — progress to eyes closed or unstable surfaces"),
                                                             tags$li("Proprioception is critical — strong ankles can still roll if your brain can't react in time"),
                                                             tags$li("Use a balance board or wobble cushion for more advanced training"),
                                                             tags$li("These exercises help your ankle react before a roll becomes a sprain")
                                                           )
                                                       )
                                                   )
                                               ),
                                               
                                               # Tip 5
                                               div(class = "ankle-tip-card", onclick = "ankleTip(this)",
                                                   div(class = "ankle-tip-header",
                                                       div(
                                                         p(class = "ankle-tip-title", "Stretch your calves regularly"),
                                                         p(class = "ankle-tip-subtitle", "Especially the lower calf (soleus)")
                                                       ),
                                                       span(class = "ankle-tip-chevron", "▼")
                                                   ),
                                                   div(class = "ankle-tip-body",
                                                       div(class = "ankle-tip-body-inner",
                                                           tags$ul(
                                                             tags$li("Soleus stretch: step one foot back, bend both knees, keep heels flat — hold 15–30 seconds"),
                                                             tags$li("Also stretch the gastrocnemius: back leg straight, lean into a wall, heel on floor"),
                                                             tags$li("Best done after exercise when muscles are warm — make it part of your cool-down"),
                                                             tags$li("Stretch both legs equally; consistency beats intensity")
                                                           )
                                                       )
                                                   )
                                               ),
                                               
                                               # Tip 6
                                               div(class = "ankle-tip-card", onclick = "ankleTip(this)",
                                                   div(class = "ankle-tip-header",
                                                       div(
                                                         p(class = "ankle-tip-title", HTML('Brace or tape if you\'ve had a previous sprain <span class="ankle-badge ankle-badge-amber">if applicable</span>')),
                                                         p(class = "ankle-tip-subtitle", "External support for high-risk activities")
                                                       ),
                                                       span(class = "ankle-tip-chevron", "▼")
                                                   ),
                                                   div(class = "ankle-tip-body",
                                                       div(class = "ankle-tip-body-inner",
                                                           tags$ul(
                                                             tags$li("A previous sprain leaves ligaments lax and increases re-injury risk — bracing helps during recovery"),
                                                             tags$li("Use a snug-but-not-tight brace; it should not restrict blood flow"),
                                                             tags$li("Gradually wean off the brace as strength and confidence return"),
                                                             tags$li("Bracing is a supplement — not a replacement — for strengthening and balance work")
                                                           )
                                                       )
                                                   )
                                               )
                                           ), # end tips list
                                           
                                           br(),
                                           
                                           # ── Quiz ──────────────────────────────────────
                                           div(style = "margin-bottom: 8px;",
                                               p(class = "ankle-section-title", "What's your ankle sprain risk?"),
                                               p(class = "ankle-section-sub", "Answer 6 quick questions to get personalized tips")
                                           ),
                                           div(class = "ankle-step-dots", id = "ankle-dots"),
                                           div(id = "ankle-q-area"),
                                           
                                           br(), br(),
                                           
                                           tags$script(HTML("
        function ankleTip(card) {
          var body = card.querySelector('.ankle-tip-body');
          var chevron = card.querySelector('.ankle-tip-chevron');
          var isOpen = body.classList.contains('open');
          document.querySelectorAll('.ankle-tip-body').forEach(function(b){ b.classList.remove('open'); });
          document.querySelectorAll('.ankle-tip-card').forEach(function(c){ c.style.borderColor = ''; });
          document.querySelectorAll('.ankle-tip-chevron').forEach(function(ch){ ch.style.transform = ''; });
          if (!isOpen) {
            body.classList.add('open');
            card.style.borderColor = '#78c2ad';
            chevron.style.transform = 'rotate(180deg)';
          }
        }

        var ankleQ = [
          {
            q: 'Have you sprained your ankle before?',
            opts: ['Yes, more than once','Yes, once','No, never'],
            scores: [3,2,0],
            feedback: [
              'Multiple sprains significantly increase re-injury risk due to stretched ligaments — bracing during activity is strongly recommended.',
              'A prior sprain leaves the ligament lax, raising your risk. Consider an ankle brace for high-intensity activities.',
              'Great — no prior history means your ligaments are at full strength.'
            ]
          },
          {
            q: 'How often do you warm up before exercise?',
            opts: ['Rarely or never','Sometimes','Always'],
            scores: [3,1,0],
            feedback: [
              'Skipping warm-ups is a top risk factor. Just 5–10 minutes of ankle circles and dynamic stretching dramatically reduces injury risk.',
              'Try to make it a consistent habit — even a short warm-up before every session makes a real difference.',
              'Excellent habit — a proper warm-up prepares your joints and reduces sprain risk.'
            ]
          },
          {
            q: 'How would you describe your balance?',
            opts: ['Poor — I wobble easily','Average','Strong — very stable on one foot'],
            scores: [3,1,0],
            feedback: [
              'Poor balance is a major risk factor. Daily single-leg balance exercises can build your proprioception quickly.',
              'There is room to improve. Try standing on one foot for 30 seconds daily and progress from there.',
              'Good balance means your ankle can react faster to uneven surfaces before a roll becomes a sprain.'
            ]
          },
          {
            q: 'Do you wear sport-appropriate footwear?',
            opts: ['No — I wear whatever is handy','Sometimes','Yes — always activity-specific shoes'],
            scores: [3,1,0],
            feedback: [
              'Wearing the wrong shoes removes a key layer of ankle protection. Sport-specific shoes provide the right lateral support.',
              'Inconsistent footwear means inconsistent protection. Try to match your shoes to the activity every time.',
              'Great — the right shoes provide crucial arch support, heel stability, and cushioning for your specific movements.'
            ]
          },
          {
            q: 'Do you stretch your calves and ankles regularly?',
            opts: ['Never','Occasionally','Yes, after most workouts'],
            scores: [2,1,0],
            feedback: [
              'Tight calves limit ankle flexibility and increase sprain risk. Add a 15-second soleus stretch into your daily routine.',
              'More consistency will pay off — tight muscles are less able to absorb sudden forces on uneven terrain.',
              'Excellent — regular calf and ankle stretching maintains the range of motion that protects your joints.'
            ]
          },
          {
            q: 'What surfaces do you typically exercise on?',
            opts: ['Uneven terrain, trails, or grass','Mixed surfaces','Flat, predictable surfaces'],
            scores: [2,1,0],
            feedback: [
              'Uneven terrain is the primary environment for ankle sprains — proprioception training is especially important for you.',
              'Mixed surfaces are common — make sure your footwear and balance training cover both indoor and outdoor scenarios.',
              'Flat surfaces carry lower risk, but it is still worth building ankle strength for everyday missteps.'
            ]
          }
        ];
        var ankleState = { cur: 0, total: 0, answers: [] };

        function ankleRenderDots() {
          var html = '';
          for (var i = 0; i < ankleQ.length; i++) {
            var cls = 'ankle-dot';
            if (i < ankleState.cur) cls += ' done';
            else if (i === ankleState.cur) cls += ' active';
            html += '<div class=\"' + cls + '\"></div>';
          }
          document.getElementById('ankle-dots').innerHTML = html;
        }

        function ankleRenderQ() {
          if (ankleState.cur >= ankleQ.length) { ankleRenderResult(); return; }
          var q = ankleQ[ankleState.cur];
          ankleRenderDots();
          var opts = q.opts.map(function(o, i) {
            return '<button class=\"ankle-opt-btn\" onclick=\"ankleSelect(' + i + ')\">' + o + '</button>';
          }).join('');
          var nextLabel = ankleState.cur + 1 < ankleQ.length ? 'Next question &rarr;' : 'See my results &rarr;';
          document.getElementById('ankle-q-area').innerHTML =
            '<div class=\"ankle-q-card\">' +
              '<p class=\"ankle-q-text\">' + (ankleState.cur + 1) + ' of ' + ankleQ.length + ' — ' + q.q + '</p>' +
              opts +
              '<div id=\"ankle-feedback\"></div>' +
            '</div>' +
            '<div class=\"ankle-nav-row\" id=\"ankle-nav\" style=\"display:none;\">' +
              '<span style=\"font-size:13px;color:#6c757d;\">' + (ankleState.cur + 1) + ' of ' + ankleQ.length + '</span>' +
              '<button class=\"ankle-nav-btn\" onclick=\"ankleNext()\">' + nextLabel + '</button>' +
            '</div>';
        }

function ankleSelect(idx) {
  var q = ankleQ[ankleState.cur];
  var score = q.scores[idx];
  ankleState.answers[ankleState.cur] = { idx: idx, score: score };
  ankleState.total = ankleState.answers.reduce(function(a, b){ return a + b.score; }, 0);

  var btns = document.querySelectorAll('.ankle-opt-btn');
  btns.forEach(function(b, i) {
    b.disabled = true;
    if (i === idx) b.className = 'ankle-opt-btn selected';
  });

  var feedbackDiv = document.createElement('div');
  feedbackDiv.className = 'ankle-feedback ankle-feedback-neutral';
  feedbackDiv.innerHTML = q.feedback[idx];
  var feedbackArea = document.getElementById('ankle-feedback');
  feedbackArea.innerHTML = '';
  feedbackArea.appendChild(feedbackDiv);

  document.getElementById('ankle-nav').style.display = 'flex';
}
        function ankleNext() { ankleState.cur++; ankleRenderQ(); }

        function ankleRenderResult() {
          var maxScore = ankleQ.reduce(function(a, q){ return a + Math.max.apply(null, q.scores); }, 0);
          var pct = Math.round((ankleState.total / maxScore) * 100);
          var level, badgeCls, headline, tips;
          if (pct <= 25) {
            level = 'Low risk'; badgeCls = 'ankle-risk-low';
            headline = 'Your habits are already working in your favor.';
            tips = ['Keep warming up consistently before every session.','Maintain your stretching routine — it is making a difference.','Consider balance drills occasionally to stay sharp on uneven terrain.'];
          } else if (pct <= 60) {
            level = 'Moderate risk'; badgeCls = 'ankle-risk-mod';
            headline = 'A few targeted changes could significantly lower your risk.';
            tips = ['Add 5 minutes of ankle circles and calf stretches to your warm-up.','Try single-leg balance stands daily — 30 seconds per leg.','Check your footwear is appropriate for your sport and replace worn shoes.'];
          } else {
            level = 'Higher risk'; badgeCls = 'ankle-risk-high';
            headline = 'Your ankles would benefit from some focused attention.';
            tips = ['Start with heel raises and resistance band ankle eversion exercises 3x/day.','Always warm up — even a 5-minute routine cuts sprain risk meaningfully.','If you have had past sprains, ask a sports medicine specialist about bracing.','Work on single-leg balance to rebuild proprioception.'];
          }
          document.getElementById('ankle-dots').innerHTML = ankleQ.map(function(){ return '<div class=\"ankle-dot done\"></div>'; }).join('');
          document.getElementById('ankle-q-area').innerHTML =
            '<div class=\"ankle-result-card\">' +
              '<span class=\"ankle-risk-badge ' + badgeCls + '\">' + level + '</span>' +
              '<p style=\"font-size:22px;font-weight:500;margin:0 0 6px;color:#2c3e50;\">' + pct + '% risk score</p>' +
              '<p style=\"font-size:14px;color:#6c757d;margin:0 0 14px;\">' + headline + '</p>' +
              '<div class=\"ankle-result-tips\">' +
                '<p style=\"font-size:13px;font-weight:500;margin:0 0 8px;color:#2c3e50;\">Your top priorities:</p>' +
                '<ul style=\"margin:0;padding-left:18px;\">' + tips.map(function(t){ return '<li>' + t + '</li>'; }).join('') + '</ul>' +
              '</div>' +
              '<div style=\"margin-top:16px;\">' +
                '<button class=\"ankle-nav-btn\" onclick=\"ankleRestart()\">Retake quiz</button>' +
              '</div>' +
            '</div>';
        }

        function ankleRestart() {
          ankleState = { cur: 0, total: 0, answers: [] };
          ankleRenderQ();
        }

        ankleRenderQ();
      "))
                                    )
                                  )
                         )
                      
              ),

             tabPanel("About Us",

                      fluidRow(
                        column(12,
                               h1("About Us",
                                  style = "text-align: center; margin-top: 40px; font-size: 40px;"),
                               h4("The team behind Sports Injury Reports",
                                  style = "text-align: center; color: grey; margin-bottom: 40px;"),
                               hr()
                        )
                      ),
                      fluidRow(
                        column(6, offset = 3, wellPanel(
                          style = "margin-bottom: 20px;",
                          tags$div(
                            style = "display: flex; align-items: center; gap: 20px;",
                            tags$img(src = "cora.jpeg",
                                     style = "width: 250px; height: 250px; object-fit: cover; border-radius: 8px; flex-shrink: 0;"),
                            tags$div(
                              h4("Cora Villere", style = "font-size: 25px; margin-top: 0;"),
                              p("2027", style = "color: grey; font-size: 20px;"),
                              p("I am a Biology major on the pre-health track, with a passion for medicine and public health.
                              Specifically, I hope to become an emergency physician. Emergency departments are on the front lines of
                              sports injuries every day, leading to a great interest in this project.",
                                style = "font-size: 20px;")
                            )
                          )
                        )),
                        column(6, offset = 3, wellPanel(
                          style = "margin-bottom: 20px;",

                          tags$div(
                            style = "display: flex; align-items: center; gap: 20px;",
                            tags$img(src = "ellery.jpeg",
                                     style = "width: 250px; height: 250px; object-fit: cover; border-radius: 8px; flex-shrink: 0;"),
                            tags$div(
                              h4("Ellery McKnight", style = "font-size: 25px; margin-top: 0;"),
                              p("2028", style = "color: grey; font-size: 20px;"),
                              p("I'm a Biology major and Data Science minor from Atlanta, GA. On campus I enjoy being part of the University Singers and am involved in greek life. In my free time I like spending time outside, hanging out with my friends, and playing the guitar.",
                                style = "font-size: 20px;")
                            )
                          )

                        )),
                        column(6, offset = 3, wellPanel(
                          style = "margin-bottom: 20px;",
                          tags$div(
                            style = "display: flex; align-items: center; gap: 20px;",
                            tags$img(src = "serenna.jpeg",
                                     style = "width: 250px; height: 250px; object-fit: cover; border-radius: 8px; flex-shrink: 0;"),
                            tags$div(
                              h4("Jou-Hsuan (Serenna) Wu", style = "font-size: 25px; margin-top: 0;"),
                              p("2029", style = "color: grey; font-size: 20px;"),
                              p("I am a Neuroscience major passionate about developing treatment for neurodegenerative diseases. On campus, I am a part of the First-Year Residential Experience Board, HOSA, the LEAD Program, Orientaion Leader, and Eco-Rep. In my free time, I enjoy playing the flute and making arts and crafts.",
                                style = "font-size: 20px;")
                            )
                          )
                        ))
                      )           
             )  
  )
)