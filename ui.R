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
                                  ))
                                )
                         )
                       )
              ),
              
              tabPanel("Sport Injuries Per Year",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(inputId = "year",
                                       label = "Year",
                                       min = 2007,
                                       max = 2024,
                                       value = 2007,
                                       sep = "")
                         ),
                         mainPanel(
                           plotlyOutput("yearly_injuries_by_sport")
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
                                       label = "Select Sport",
                                       choices = unique(sport_injuries_by_age$sport_or_activity),
                                       selected = NULL
                           )
                         ),
                         mainPanel(plotOutput("sport_injuries_by_age"))
                       ))
  )
)
    

