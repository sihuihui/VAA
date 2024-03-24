pacman::p_load(tidyverse, lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip, workflows)

weatherdata <-read_rds("data/weather_data_imputed.rds")


ui <- fluidPage(
  navbarPage("Forecasting Modules",
  tabPanel("Exponential Smoothing",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "sh_station",
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
               selectInput(inputId = "sh_variable",
                           label = "Select the variable to forecast",
                           choices = c("Mean Temperature" = "mean_monthly_temperature",
                                       "Maximum Temperature" = "max_monthly_temperature",
                                       "Minimum Temperature" = "min_monthly_temperature",
                                       "Total Rainfall" = "monthly_rainfall"),
                           selected = "mean_monthly_temperature"),
               
               hr(), 
               checkboxGroupInput(inputId = "sh_decompose",
                           label = "Select the STL decompositions to visualise",
                           choices = c("Observed" = "observed",
                                       "Season" = "season",
                                       "Trend" = "trend",
                                       "Remainder" = "remainder",
                                       "Seasonal Adjusted" = "seasadj")),
               
               hr(),
               actionButton(inputId = "sh_plotdata",
                            label = "Show my Data!"),
              
               
               hr(),
               sliderInput("sh_traindata",
                           label = "Select the amount of Training Data to use", 
                           min = 0.6,
                           max = 1,
                           value = 0.8,
                           step = 0.05),
               
               hr(),
               sliderInput("sh_forecasthorizon",
                           label = "Select the Forecast Horizon", 
                           min = 1,
                           max = 120,
                           value = 36,
                           step = 1),
               
               hr(), 
               selectInput(inputId = "sh_seasonalperiod",
                           label = "Select a seasonal frequency",
                           choices = c("Auto" = "auto",
                                       "1 month" = "1",
                                       "3 months" = "3",
                                       "12 months" = "12"),
                           selected = "auto"),
               
               hr(),
               radioButtons(inputId = "sh_error",
                            label = "Select the form of error term",
                            choices = c("Auto" = "auto",
                                        "Additive" = "additive",
                                        "Multiplicative" = "multiplicative"),
                            selected = "auto"), 
               hr(),
               radioButtons(inputId = "sh_trend",
                            label = "Select the form of trend term",
                            choices = c("Auto" = "auto",
                                        "Additive" = "additive",
                                        "Multiplicative" = "multiplicative"),
                            selected = "auto"), 
               hr(),
               radioButtons(inputId = "sh_season",
                            label = "Select the form of season term",
                            choices = c("Auto" = "auto",
                                        "Additive" = "additive",
                                        "Multiplicative" = "multiplicative",
                                        "None" = "none"),
                            selected = "auto"), 
               hr(),
               radioButtons(inputId = "sh_damp",
                            label = "Select the form of damping term",
                            choices = c("Auto" = "auto",
                                        "Damped" = "damped",
                                        "None" = "none"),
                            selected = "auto"), 
               hr(),
               actionButton(inputId = "sh_forecast",
                            label = "Forecast!")
               
             ), 
             mainPanel(
               plotOutput("sh_SelectedDataPlot"),
               plotOutput("sh_DecompositionPlot"),
               plotOutput("sh_ValidationPlot"),
               plotOutput("sh_ForecastPlot"),
               plotOutput("sh_ResidualsPlot"),
               DT::dataTableOutput("sh_AccuracyTable"),
               DT::dataTableOutput("sh_SelectedDataTable")
             )
           )),
  tabPanel("ARIMA")
  )
)


server <- function(input, output) {
  
## Filter Data based on selected station and weather variable 
  selectedData <- eventReactive(input$sh_plotdata, {
    weatherdata %>%
      filter(station %in% input$sh_station)%>%
      # mutate(MTHYEAR = my(as.Date(tdate)))
      #select(tdate,input$sh_variable)
    select(station, date = tdate, value = input$sh_variable)
  })
  
## Plot filtered Data based on selected variable 
  output$sh_SelectedDataPlot <- renderPlot({
    req(selectedData())
    ggplot(data = selectedData(),
           aes_string(x = "date", y = "value")) +
      geom_line() + 
      theme_ipsum_rc()
  })
  
  #output$sh_SelectedDataTable <- renderTable({
    #req(selectedData())
    #selectedData()
    #})
  
  #output$sh_DecompositionPlot <- renderPlot({
    #req(selectedData())
    #timetk::plot_stl_diagnostics(selectedData(), as.Date(tdate), input$sh_variable,
                         #.feature_set = input$sh_decompose)
  #})
    

## split the data based on the training proportion chosen 
  splits <- eventReactive(input$sh_forecast,{
    req(selectedData())
    initial_time_split(selectedData(), prop = input$sh_traindata)
  })
  
  recipe_trg <- reactive({
    recipe(value ~ date, training(splits())) %>%
      step_dummy(all_nominal())
  })
  
  #recipe_trg <- recipe_trg %>% 
    #step_dummy(all_nominal())
  
  train_data <- reactive({ 
    training(splits()) 
  })
  
  test_data <- reactive({ 
    testing(splits()) 
  })
  
## Fit the model based on the parameters 
  model_fit_ets <- reactive({
    exp_smoothing(
      seasonal_period = input$sh_seasonalperiod, 
      error = input$sh_error, 
      trend = input$sh_trend,
      season = input$sh_season, 
      damping = input$sh_damp
    ) %>%
      set_engine(engine = "ets") %>%
      fit(value ~ date, data = train_data())
  })

## Add Fitted Model to a Model table 
  model_ets_table <- modeltime_table(model_fit_ets)

## Calibrate model to test data
  calibration_ets <- model_ets_table %>%
      modeltime_calibrate(new_data = test_data())

  calibration_results <- calibration_ets %>%
      modeltime_forecast(new_data = test_data(),
                         actual_data = selectedData())

## Plot forecasted and actual test data

  
  }
  

# Run the application 
shinyApp(ui = ui, server = server)
