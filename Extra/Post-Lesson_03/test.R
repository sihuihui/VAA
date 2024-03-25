pacman::p_load(tidyverse, shiny, bslib, 
               lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip, workflows, patchwork, thematic, showtext, glue, bsicons,
               tmap
               )

weatherdata <-read_rds("data/weather_data_imputed.rds")

weatherdata_summary <-  weatherdata %>%
  group_by(station)%>%
  summarise(average_temperature = round(mean(mean_monthly_temperature),1),
            max_temperature = max(max_monthly_temperature),
            min_monthly_temperature = min(min_monthly_temperature),
            total_rainfall = round(sum(monthly_rainfall), 0)) 

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
    title = "Highest Monthly Temp",
    value = textOutput("hotyear"),
    showcase = bs_icon("thermometer-high"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#fa2e05", fg = "#f0f2f5"),
    max_height = "130px"
  ),
  
  #2nd value box 
  value_box(
    title = "Coolest Monthly Temp",
    value = textOutput("coolyear"),
    showcase = bs_icon("thermometer-low"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#e7ebbc", fg ="#1a1818"),
    max_height = "130px"
    #p("At XX station on MM YYYY")
  ),
  
  #3rd value box 
  value_box(
    title = "Average Monthly Temp",
    value = textOutput("avgtemp"),
    showcase = bs_icon("thermometer-sun"),
    showcase_layout = "top right",
    theme = "orange",
    max_height = "130px"
  ),
  
  #4th value box 
  value_box(
    title = "Lowest Monthly Rainfall",
    value = textOutput("lowrain"),
    showcase = bs_icon("cloud-drizzle"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#a1d5f0", fg = "#1a1818"),
    max_height = "130px"
  ),
  
  #5th value box 
  value_box(
    title = "Highest Monthly Rainfall",
    value = textOutput("highrain"),
    showcase = bs_icon("cloud-rain-heavy"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#065299", fg = "#f0f2f5"),
    max_height = "130px"
  ),
  
  #6th value box 
  value_box(
    title = "Average Monthly Rainfall",
    value = textOutput("avgrain"),
    showcase = bs_icon("umbrella-fill"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#6dc2a3",fg = "#1a1818"),
    max_height = "130px"
  )
)


ui <- page_navbar(
  title = "Rain or Shine: Exploring the mysteries of Singapore Weather",
  nav_spacer(),
  nav_panel(title = "Dashboard",
            layout_columns(
              col_widths = c(5,7,8,4),
              row_heights = c(2,3), 
              
              #value boxes 
              card(
                layout_column_wrap(
                  #width = "250px",
                  !!!vbs)
              ),
              
              # datatable 
              card(
                card_body(DT::dataTableOutput(outputId = "dtTable"))
              ),
              
              #map 
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
                      selected = "mean_monthly_temperature"),
                    card_body(tmapOutput("mapplot"))
                  ))
              ), 
              
              #plot 
              card(
                card_body(plotOutput(outputId = "stationplot"))
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
    paste(max(weatherdata$max_monthly_temperature),"°C")
  })
  
  output$coolyear <- renderText({
    paste(min(weatherdata$min_monthly_temperature),"°C")
  })
  
  output$avgtemp <- renderText({
    paste(round(mean(weatherdata$mean_monthly_temperature),1),"°C")
  })
  
  output$lowrain <- renderText({
    paste(min(weatherdata$monthly_rainfall),"mm")
  })
  
  output$highrain <- renderText({
    paste(max(weatherdata$monthly_rainfall),"mm")
  })
  
  output$avgrain <- renderText({
    paste(round(mean(weatherdata$monthly_rainfall),0),"mm")
  })
  
  output$dtTable <- DT::renderDataTable({
    DT::datatable(data = weatherdata_summary,
                  options = list(pageLength = 3),
                  rownames = FALSE,
                  colnames = c('Station', 'Mean Temp', 'Max Temp', 'Min Temp', 'Total Rainfall'),
                  DT:::DT2BSClass(c('compact', 'cell-border')))

  })
  
}

shinyApp(ui, server)