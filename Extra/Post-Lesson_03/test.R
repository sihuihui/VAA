pacman::p_load(tidyverse, shiny, bslib, 
               lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip, workflows, patchwork, thematic, showtext, glue, bsicons)

weatherdata <-read_rds("data/weather_data_imputed.rds")

# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  bootswatch = "flatly",
  base_font = font_google("Roboto")
) 

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")


ui <- page_navbar(
  title = "Rain or Shine: Exploring the mysteries of Singapore Weather",
  nav_spacer(),
  nav_panel(title = "Dashboard", #p(
    
    layout_columns(
      fill = FALSE, 
      value_box(
        title = "1st value",
        value = "123",
        showcase = bs_icon("bar-chart"),
        theme = "purple"
      ),
      value_box(
        title = "2nd value",
        value = "456",
        showcase = bs_icon("graph-up"),
        theme = "teal"
      ),
      value_box(
        title = "3rd value",
        value = "789",
        showcase = bs_icon("pie-chart"),
        theme = "pink"
      ),
      value_box(
        title = "4th value",
        value = "789",
        showcase = bs_icon("pie-chart"),
        theme = "pink"
      )
    ),
    #layout_columns(
      #cards[[1]], cards[[2]]
    #),
    #cards[[3]],
  
    
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
  
}

shinyApp(ui, server)