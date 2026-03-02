library(shiny)
library(plotly)
library(bslib)


fluidPage(
  theme =  bs_theme(bootswatch ="minty"),
  tabsetPanel(
    tabPanel("Yearly Injuries by Sport",
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
    tabPanel("Yearly Injuries by Age",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "age_group",
                             label = "select age group",
                             choices = c("0_to_4",
                                         "4_to_15",
                                         "14_to_24",
                                         "25_to_64",
                                         "65_or_over"
                                         )
                             )
               ),
               mainPanel(
                 plotlyOutput("yearly_injuries_by_age")
               )
             )
             )
  )
)