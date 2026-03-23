library(shiny)
library(plotly)
library(bslib)
library(leaflet)

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
                       fluidRow(
                         column(12,
                                h1("Welcome!", style = "text-align: center; margin-top: 50px; font-size: 40px;"),
                                h4("Exploring Sports Injury Data from 2007 to 2024",
                                   style = "text-align: center; color: grey;"),
                                hr(),
                                fluidRow(
                                  column(4, wellPanel(
                                    h4("Sport Injuries Per Year"),
                                    p("See which sports cause the most injuries each year"),
                                    actionButton("go_sport", "Explore", class = "btn-primary")
                                  )),
                                  column(4, wellPanel(
                                    h4("Injuries by Age Group"),
                                    p("Explore how injuries vary across age groups"),
                                    actionButton("go_age", "Explore", class = "btn-primary")
                                  )),
                                  column(4, wellPanel(
                                    h4("Sport Injuries by Age"),
                                    p("Break down injuries by sport and age group"),
                                    actionButton("go_sport_age", "Explore", class = "btn-primary")
                                  )),
                                  tags$div(style = "margin-top: 16px; width: 100%;"),
                                  column(4, wellPanel(
                                    h4("Favorite Sport by State"),
                                    p("Explore each state's favorite sport and corresponding injury facts"),
                                    actionButton("go_state", "Explore", class = "btn-primary")
                                  )),
                                  column(4, wellPanel(        # ← add this
                                    h4("About Us"),
                                    p("Meet the team behind Sports Injury Reports"),
                                    actionButton("go_about", "Meet the Team", class = "btn-primary")
                                  ))
                                ),
                                h3("About", style = "text-align: center; margin: 30px auto 10px; font-size: 40px;"),
                                hr(style = "width: 100px; border-top: 2px solid #78c28d; margin: 10px auto 20px;"),
                                p("This dashboard was created by Ellery Mcknight, Serenna Wu, and Cora Villere.
            Our goal was to explore sports and recreational injury data collected over the past couple decades.
            We have examined injury trends across different sports, age groups, and years.
            Our goal is to make injury data accessible and understandable for parents, coaches, and athletes alike.",
                                  style = "text-align: center; font-size: 25px; max-width:900px; margin: 20px auto;")
                         )
                       )
              ),
              
              tabPanel("Sport Injuries Per Year",
                       fluidRow(
                         column(12,
                                h1("See which sports cause the most injuries each year",
                                   style = "text-align: center; margin-top: 50px; font-size: 40px;"),
                                h4("Instructions...",
                                   style = "text-align: center; color: grey;"),
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 4,
                                    h4("Filter by Year"),
                                    sliderInput(inputId = "year",
                                                label = "Year",
                                                min = 2007,
                                                max = 2024,
                                                value = 2007,
                                                sep = "",
                                                animate = animationOptions(
                                                  interval = 1000,
                                                  loop = TRUE,
                                                  playButton = "▶ Play",
                                                  pauseButton = "⏸ Pause"
                                                )
                                    ),
                                    hr(),
                                    p("Use the slider or press Play to animate injuries over time.",
                                      style = "color: gray; font-size: 15px;"),

                                    br(),
                                    wellPanel(
                                      style = "background: white; border: 1px solid #ddd; border-radius: 8px; padding: 12px;",
                                      uiOutput("top_sport_boxes")
                                    )
                                  ),
                                  mainPanel(
                                    width = 8,
                                    div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; height: 950px; overflow: hidden; margin-bottom: 30px;",
                                        plotlyOutput("yearly_injuries_by_sport", height = "1000px")
                                    )
                                  )
                                )
                         )
                       )
              ),
              
              
              tabPanel("Injuries By Age Group",
                       fluidRow(
                         column(12, 
                                h1("Explore how injuries vary across age groups",
                                   style = "text-align: center; margin-top: 50px; font-size: 40px;"),
                                h4("Instructions...",
                                   style = "text-align: center; color: grey;"),
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
                         column(12,
                                h1("Break down injuries by sport and age group",
                                   style = "text-align: center; margin-top: 50px; font-size: 40px;"),
                                h4("Instructions...",
                                   style = "text-align: center; color: grey;"),
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
             
              tabPanel("Favorite Sport by State",
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
                         p(style = "margin: 0; color: #2c3e50; font-size: 16px;",
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
             tabPanel("About",
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
                        column(4, wellPanel(
                          tags$img(src = "person1.jpg",
                                   style = "width: 100%; border-radius: 50%; margin-bottom: 15px;"),
                          h4("Cora Villere", style = "text-align: center;"),
                          p("2027", style = "text-align: center; color: grey; font-size: 12px;"),
                          p("I am a Biology major on the pre-health track, with a passion for medicine and public health.
                            Specifically, I hope to become an emergency physician. Emergency departments are on the front lines of
                            sports injuries every day, leading to a great interest in this project.",
                            style = "text-align: center; font-size: 13px;")
                        )),
                        column(4, wellPanel(
                          tags$img(src = "person2.jpg",
                                   style = "width: 100%; border-radius: 50%; margin-bottom: 15px;"),
                          h4("Name Here", style = "text-align: center;"),
                          p("Year", style = "text-align: center; color: grey; font-size: 12px;"),
                          p("Blurb about yourself here.",
                            style = "text-align: center; font-size: 13px;")
                        )),
                        column(4, wellPanel(
                          tags$img(src = "person3.jpg",
                                   style = "width: 100%; border-radius: 50%; margin-bottom: 15px;"),
                          h4("Serenna Wu", style = "text-align: center;"),
                          p("2029", style = "text-align: center; color: grey; font-size: 12px;"),
                          p("I am a Neuroscience major pursuing a career in...",
                            style = "text-align: center; font-size: 13px;")
                        ))
                      )             
             )  
  )
)