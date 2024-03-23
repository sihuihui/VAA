pacman::p_load(tidyverse, lubridate, DT, ggplot2, plotly, ggthemes, timetk, modeltime, tidymodels, xgboost, recipes, parsnip, earth, shiny, shinybusy, shiny.semantic, shinydashboardPlus)

weatherdata <-read_rds("data/weather_data_imputed.rds")


forecastui <- fluidPage(
  navbarPage("Forecasting Modules",
  tabPanel("Exponential Smoothing",
           sidebarLayout(
             sidebarPanel(
               accordion(id = "accordion1",
                         accordionItem(
                           title = "Data Selection",
                           collapsed = FALSE, 
                           
                           selectizeInput(
                             inputId = "shStation",
                             label = "Select a weather station to forecast",
                             choices = c("Admiralty",
                                         "Ang Mo Kio",
                                         "Changi", 
                                         "Choa Chu Kang (South)",
                                         "Clementi",
                                         "East Coast Parkway",
                                         "Jurong Island",
                                         "Jurong (West)",
                                         "Newton",
                                         "Pasir Panjang", 
                                         "Sentosa Island",
                                         "Tai Seng",
                                         "Tuas South"),
                             selected = c("Admiralty"),
                             options = list(maxItems = 1, create = TRUE)),
                           
                           selectizeInput(
                             inputId = "shVariable",
                             label = "Select the variable to forecast",
                             choices = c("Mean Temperature" = "mean_monthly_temperature",
                                         "Maximum Temperature" = "max_monthly_temperature",
                                         "Minimum Temperature" = "min_monthly_temperature",
                                         "Total Rainfall" = "monthly_rainfall"),
                             selected = c("mean_monthly_temperature"),
                             options = list(maxItems = 1, create = TRUE)),
                           hr(),
                           
                           sliderInput(
                             "shTrainData",
                             label = "Select the amount of Training Data to use",
                             min = 0.6,
                             max = 1,
                             value = 0.8,
                             step = 0.05),
                           hr(),
                           
                           sliderInput(
                             "shForecastHorizon",
                             label = "Select the forecast horizon (months)",
                             min = 1,
                             max = 120,
                             value = 36,
                             step = 1),
                           hr(),
                           
                           actionButton("sh_TrainModel", 
                                        "Train My Model!")
                           
                           
                         ))

               
             ), 
             mainPanel(
               plotOutput("shSelectedData")
             )
           )
           ),
  tabPanel("ARIMA")
  )
)


forecastserver <- function(input, output, session) {
  selectedData <- eventReactive(input$sh_PlotMyData, {
    weatherdata %>%
      filter(station %in% input$shStation)
  })
  
  output$shSelectedData <- renderPlot({
    req(selectedData())
    data_to_plot <- selectedData()
    ggplot(data = data_to_plot,
           aes(x = as.Date(tdate), y = .data[[input$shVariable]])) +
      geom_line() + 
      theme_ipsum_rc()
  })
             
  }
  
    


# Run the application 
shinyApp(ui = forecastui, server = forecastserver)
