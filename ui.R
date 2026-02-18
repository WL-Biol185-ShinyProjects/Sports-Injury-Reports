library(shiny)


fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", 
                  label = "Year",
                  min = 2007,
                  max = 2024,
                  value = 2007
                  )
    ),
    mainPanel(
      plotOutput("Yearly Injuries By Sport")
    )
  )
)