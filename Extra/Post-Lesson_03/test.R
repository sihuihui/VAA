pacman::p_load(tidyverse, shiny, bslib, 
               lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip, workflows, patchwork, thematic, showtext, glue, bsicons)

weatherdata <-read_rds("data/weather_data_imputed.rds")

# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  bslib = "shiny",
  base_font = font_google("Roboto")
) 

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")


vbs <- list(
  fill = FALSE, 
  #1st value box 
  value_box(
    title = "Highest Monthly Temperature Experienced",
    value = textOutput("hotyear"),
    showcase = bs_icon("thermometer-high"),
    theme = "orangered"
  ),
  
  #2nd value box 
  value_box(
    title = "Coolest Monthly Temperature Experienced",
    value = textOutput("coolyear"),
    showcase = bs_icon("thermometer-low"),
    theme = "lightgoldenrod1"
  ),
  
  #3rd value box 
  value_box(
    title = "Average Monthly Temperature from 2014 to 2023",
    value = textOutput("avgtemp"),
    showcase = bs_icon("thermometer-sun"),
    theme = "orange"
  ),
  
  #4th value box 
  value_box(
    title = "Lowest Monthly Rainfall",
    value = textOutput("lowrain"),
    showcase = bs_icon("cloud-drizzle"),
    theme = "lightsteelblue1"
  ),
  
  #5th value box 
  value_box(
    title = "Highest Monthly Rainfall",
    value = textOutput("highrain"),
    showcase = bs_icon("cloud-rain-heavy"),
    theme = "steelblue3"
  ),
  
  #6th value box 
  value_box(
    title = "Average Monthly Rainfall",
    value = textOutput("avgrain"),
    showcase = bs_icon("umbrella-fill"),
    theme = "lightskyblue"
  )
)


ui <- page_navbar(
  title = "Rain or Shine: Exploring the mysteries of Singapore Weather",
  nav_spacer(),
  nav_panel(title = "Dashboard", 
            
            card(
              layout_column_wrap(
                width = "250px",
                !!!vbs)
              ),

    card(
      height = 350,
      card_header("Use this to toggle the map"),
      layout_sidebar(
        sidebar = sidebar(
          selectInput(
            inputId = "map_input",
            label = "Indicate the variable to display",
            choices = c("Mean Temperature" = "mean_monthly_temperature",
                        "Maximum Temperature" = "max_monthly_temperature",
                        "Minimum Temperature" = "min_monthly_temperature",
                        "Total Rainfall" = "monthly_rainfall"),
            selected = "mean_monthly_temperature")
        )
      )
  )
  ),
  
  nav_panel(title = "Comparison Sandbox", p("Second page content.")),
  nav_panel(title = "Comparison Analysis", p("Third page content.")),
  nav_menu(
    title = "Forecasting",
    nav_panel(title = "Exponential Smoothing", p("ETS.")),
    nav_panel(title = "ARIMA", p("Arima.")),
    align = "left"
      )
    )
  
server <- function(input, output){
  
  output$hotyear <- renderText({
    max(weatherdata$max_monthly_temperature)
  })
  
  output$coolyear <- renderText({
    min(weatherdata$min_monthly_temperature)
  })
  
  output$avgtemp <- renderText({
    round(mean(weatherdata$mean_monthly_temperature),1)
  })
  
  output$lowrain <- renderText({
    min(weatherdata$monthly_rainfall)
  })
  
  output$highrain <- renderText({
    max(weatherdata$monthly_rainfall)
  })
  
  output$avgrain <- renderText({
    round(mean(weatherdata$monthly_rainfall),0)
  })
  
}

shinyApp(ui, server)