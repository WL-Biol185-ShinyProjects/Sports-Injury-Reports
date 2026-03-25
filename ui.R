library(shiny)
library(plotly)
library(bslib)
library(leaflet)
library(markdown)

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
                             "Select an age group and one or more sports from the sidebar to compare injury counts across different age group. Use the sport selector to add or remove sports from the chart.")
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
                                                multiple = TRUE)
                                  ),
                                  mainPanel(
                                    width = 9,
                                    div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 30px;",
                                        plotOutput("yearly_injuries_by_age")
                                    )
                                  )
                                )
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
                                    plotOutput("age_legend", height = "180px")
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

           
              tabPanel("Injury Prevention",
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
                                    p(style = "color: white; opacity: 0.9; font-size: 20px; margin: 0;",
                                      "Taking small steps before and after activity can save you weeks of recovery time.")
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
                                            tags$li(tags$a(href = "https://www.safekids.org", target = "_blank",
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
                                            tags$li(tags$a(href = "https://www.ncaa.org/sports/2014/10/6/sport-science-institute.aspx",
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
                              h4("Cora Villere", style = "margin-top: 0;"),
                              p("2027", style = "color: grey; font-size: 12px;"),
                              p("I am a Biology major on the pre-health track, with a passion for medicine and public health.
                              Specifically, I hope to become an emergency physician. Emergency departments are on the front lines of
                              sports injuries every day, leading to a great interest in this project.",
                                style = "font-size: 13px;")
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
                              h4("Ellery Mcknight", style = "margin-top: 0;"),
                              p("2028", style = "color: grey; font-size: 12px;"),
                              p("I'm a biology major and data science minor from Atlanta, GA. On campus I enjoy being part of the University Singers and am involved in greek life. In my free time I like spending time outside and playing the guitar.",
                                style = "font-size: 13px;")
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
                              h4("Serenna Wu", style = "margin-top: 0;"),
                              p("2029", style = "color: grey; font-size: 12px;"),
                              p("Blurb about yourself here.",
                                style = "font-size: 13px;")
                            )
                          )
                        ))
                      )           
             )  
  )
)