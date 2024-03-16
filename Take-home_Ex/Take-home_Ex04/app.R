pacman::p_load(shiny,tidyverse, lubridate, knitr, DT, ggplot2, plotly, ggthemes, 
               ggfortify, forecast, MLmetrics, tsbox, xts, imputeTS, tseries, hrbrthemes)

temp <-read_rds("data/monthly_temp.rds")

ui <- fluidPage(
    titlePanel("Prototype for Forecasting Module"),
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "shstation",
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
          hr(),
          selectInput(inputId = "shtemp",
                      label = "Select the type of temperature reading to forecast",
                      choices = c("Mean Temperature" = "mean_temperature",
                                  "Maximum Temperature" = "maximum_temperature",
                                  "Minimum Temperature" = "minimum_temperature"),
                      selected = "mean_temperature"),
          hr(),
          actionButton(inputId = "temp_plot",
                       label = "Show my Temperature Data!"),
          hr(),
          radioButtons("shdataimpute", 
                        "Select a Missing Data Imputation Method",
                       c("Moving Average (Exponential)" = "exponential",
                         "Moving Average (Linear)" = "linear",
                         "Moving Average (Simple)" = "simple",
                         "Kalman Smoothing (ARIMA)" = "auto.arima",
                         "Kalman Smoothing (StrucTS)" = "StructTS")),
          hr(),
          actionButton(inputId = "temp_imputed",
                       label = "Show my Temperature Data Again!"),
          hr(),
          radioButtons("shforecast", 
                       "Select a Forecasting Method",
                       c("State Space Model" = "ets",
                         "Holt-Winters' Model (Additive Seaonality)" = "hwa",
                         "Holt-Winters' Model (Multiplicative Seaonality)" = "hwm",
                         "ARIMA" = "arima")),
          hr(),
          sliderInput(inputId = "forecastduration",
                      label = "Select the number of months to forecast",
                      min = 1,
                      max = 120,
                      value = 24,
                      step = 1),
          hr(),
          actionButton(inputId = "temp_forecast",
                       label = "Forecast Temperature Data")
        ),
        mainPanel("main panel",
                  fluidRow(
                    column(6, plotOutput(outputId = "tempdata",
                                         width = "300px",
                                         height = "300px")),
                    column(6, plotOutput(outputId = "forecastdata",
                                         width = "300px",
                                         height = "300px")),
                    
                  )
        )
    )
)


server <- function(input, output) {

  ######################################### Select data based on user input ###################################################### 

  
  dataInput <- eventReactive(input$temp_plot, {
    temp %>% 
      filter(temp()$station %in% input$shstation) %>%
      select(input$shtemp)
  })
  
  output$tempdata <- renderPlot({
    ggplot(data = dataInput(),
           aes_string(x = dataInput$year_month,
                      y = input$shtemp)) +
      geom_line() +
      theme_clean() 
  })
             
  }
  
    


# Run the application 
shinyApp(ui = ui, server = server)
