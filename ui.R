library(shiny)
library(plotly)
library(bslib)

yearly_injuries_final <- read.csv("yearly_injuries_final.csv") %>%
  
  #Fixing the Table
  mutate(injuries = as.numeric(gsub(",", "", injuries)),
         year = as.numeric(year),
         sport_or_activity = trimws (sport_or_activity),
         sport_or_activity = stringr::str_squish(sport_or_activity),
         ) %>%
  group_by(sport_or_activity, year) %>%
  summarise(injuries = sum(injuries, na.rm = TRUE), .groups = "drop")

fluidPage(
  theme =  bs_theme(bootswatch ="minty"),
  tabsetPanel(
    tabPanel("Sport Injuries Per Year",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "year",
                             label = "year",
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
                             label = "select age group",
                             choices = c("0 to 4" = "0_to_4",
                                         "4 to 15" = "4_to_15",
                                         "14 to 24" = "14_to_24",
                                         "25 to 64" = "25_to_64",
                                         "65 or over" = "65_or_over"
                                         )
                             )
               ),
               mainPanel(
                 plotlyOutput("yearly_injuries_by_age")
               )
             )
             ),
    tabPanel("Sport Injuries by Age",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "sport_or_activity",
                             label = "select sport",
                             choices = unique(yearly_injuries_final$sport_or_activity),
                             selected = NULL
                             )
               ),
               mainPanel(plotlyOutput("sport_injuries_by_age"))
             ))
  )
)