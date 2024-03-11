pacman::p_load(shiny,tidyverse, lubridate, knitr, DT, ggplot2, plotly, ggthemes, 
               ggfortify, forecast, MLmetrics, tsbox, xts, imputeTS, tseries, hrbrthemes)

temp <-read_rds("Take-home_Ex/Take-home_Ex04/data/temp_clean.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Prototype for Forecasting Module"),
    sidebarLayout(
        sidebarPanel(
          selectInput(inputID ="SHstation",
                      label = "Select a weather station",
                      choices = c("Admiralty" = "Admiralty",
                                  "Ang Mo Kio" = "Ang Mo Kio",
                                  "Changi" = "Changi", 
                                  "Choa Chu Kang (South)" = "Choa Chu Kang (South)",
                                  "Clementi" = "Clementi",
                                  "East Coast Parkway" = "East Coast Parkway",
                                  "Jurong Island" = "Jurong Island",
                                  "Jurong (West)" = "Jurong (West)",
                                  "Newton" = "Newton",
                                  "Pasir Panjang" = "Pasir Panjang", 
                                  "Sentosa Island"  = "Sentosa Island",
                                  "Tai Seng" = "Tai Seng",
                                  "Tuas South" = "Tuas South"),
                      selected = "Admiralty"),
          hr()
        ),
        mainPanel(
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

 
    }


# Run the application 
shinyApp(ui = ui, server = server)
