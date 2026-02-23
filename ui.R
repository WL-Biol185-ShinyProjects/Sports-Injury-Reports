library(shiny)
library(plotly)



fluidPage(
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
    tabPanel("Injuries by Age")
  )
)