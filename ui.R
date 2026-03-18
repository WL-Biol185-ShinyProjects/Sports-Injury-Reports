library(shiny)
library(plotly)
library(bslib)


  
sport_injuries_by_age <-read.csv("yearly_injuries_final.csv") %>%
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws (sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity)
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

fluidPage(
  theme =  bs_theme(bootswatch ="minty"),
  h2("Sports Injury Reports", style = "text-align: center; font-size: 50px;"),
  tags$style(HTML("                
    .slider-animate-container {
      margin-top: 10px;
    }
  ")),
  
  tabsetPanel(id = "tabs",  # give tabsetPanel an id
              tabPanel("Home",
                       fluidRow(
                         column(12,
                                h1("Welcome!",
                                   style = "text-align: center; margin-top: 50px;font-size: 40px;"),
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
                                    p("Explore each state's favorite sport and correponding injury facts"),
                                    actionButton("go_state", "Explore", class = "btn-primary")
                                  ))
                                ),
                                h3("About",
                                   style = "text-align: center; 
                                margin: 30px auto 10px; 
                                font-size: 40px;"),
                                hr(style = "width: 100px; 
                                border-top: 2px solid #78c28d; 
                                margin: 10px auto 20px;"),
                                p("This dashboard was created by Ellery Mcknight, Serenna Wu, and Cora Villere. 
                                Our goal was to explore sports and recreational injury data collected over the past couple decades. 
                                  We have examined injury trends across different sports, age groups, and years.
                                  Our goal is to make injury data accessible and understandable for parents, coaches, and athletes alike.",
                                  style = "text-align: center; 
                                  font-size: 25px; 
                                  max-width:900px; 
                                  margin: 20px auto;")
                         )
                       )
              ),
              
              tabPanel("Sport Injuries Per Year",
                       sidebarLayout(
                         sidebarPanel(
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
                             style = "color: gray; font-size: 15px;")
                         ),
                         mainPanel(
                           plotlyOutput("yearly_injuries_by_sport", height = "500px")
                         )
                       )
              ),
              tabPanel("Injuries by Age Group",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "age_group",
                                       label = "Select Age Group",
                                       choices = c("0 to 4" = "X0_to_4",
                                                   "4 to 15" = "X4_to_15",
                                                   "14 to 24" = "X14_to_24",
                                                   "25 to 64" = "X25_to_64",
                                                   "65 or over" = "X65_or_over"
                                       )
                           ),
                           selectInput(inputId = "sport_or_activity",
                                       label = "Select Sport(s)",
                                       choices = unique(injuries_by_agegroup$sport_or_activity),
                                       selected = unique(injuries_by_agegroup$sport_or_activity)[1],
                                       multiple = TRUE)
                         ),
                         mainPanel(
                           plotOutput("yearly_injuries_by_age")
                         )
                       )
              ),
              tabPanel("Sport Injuries by Age",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "sport_or_activity",
                                       label = "Select Sport(s)",
                                       choices = unique(injuries_by_agegroup$sport_or_activity),
                                       selected = unique(injuries_by_agegroup$sport_or_activity)[1],
                                       multiple = TRUE),
                         ),
                         mainPanel(uiOutput("sport_injuries_by_age")),
                       )),
              tabPanel("Favorite Sport by State",
                                leafletOutput("sport_map", height = "600px")
                       )
  )
)
    

