library(shiny)


fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", 
                  label = "year",
                  min = 2007,
                  max = 2024,
                  value = 2007
                  )
    ),
    mainPanel(
      plotOutput("yearly_injuries_by_sport")
    )
  )
)

