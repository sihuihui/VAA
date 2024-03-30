## Loading packages
pacman::p_load(shiny,ggplot2,bslib,gridlayout,tidyverse,ggridges,ggrepel,ggthemes,ggstatsplot,
               ggsignif,hrbrthemes,patchwork,dplyr, gifski, gapminder,plotly,gganimate,ggiraph,magick,car)

## Loading datasets
weather_data_imputed <-read_rds("data/weather_data_imputed.rds")
weather_data_detailed <- read_csv("data/weather_data_detailed.csv")
temp_year <- read_csv("data/temp_year.csv")
temp_month <- read_csv("data/temp_month.csv")
temp_stn <- read_csv("data/temp_stn.csv")
rainfall_data_year <- read_csv("data/rainfall_data_year.csv")
rainfall_data_month <- read_csv("data/rainfall_data_month.csv")
rainfall_data_stn <- read_csv("data/rainfall_data_stn.csv")
combined_data <- read_csv("data/combined_data.csv")
combined_data2 <- read_csv("data/combined_data2.csv")
combined_data3 <- read_csv("data/combined_data3.csv")

## hline values for cycle plots

hline.data <- rainfall_data_month %>%
  group_by(month) %>%
  summarise(avgvalue = mean(monthly_rainfall))

hline_mean_temp.data <- temp_month %>%
  group_by(year) %>%
  summarise(avgvalue = mean(meantemp))

hline_max_temp.data <- temp_month %>%
  group_by(year) %>%
  summarise(avgvalue = mean(maxtemp))

hline_min_temp.data <- temp_month %>%
  group_by(year) %>%
  summarise(avgvalue = mean(mintemp))

## UI

ui <- page_navbar(
  title = "Rain, Hail or Shine: Unveiling Mysteries of the Sky",
  selected = "Exploratory Data Analysis",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Analysis",
    id = "tabset-default-id",
    open = "closed",
    radioButtons(
      inputId = "choiceAnalysis",
      label = "Choice of Analysis",
      choices = list(
        "Dashboard" = "a",
        "Exploratory Data Analysis" = "b",
        "Confirmatory Data Analysis" = "value3",
        "Timeseries Forecasting" = "value4"
      ),
      width = "100%"
    )
  ),
  id = "tabset-default-id",
  nav_panel(
    title = "Exploratory Data Analysis",
    tabsetPanel(
      nav_panel(
        title = "Overview of Rainfall",
        grid_container(
          layout = c(
            "area0 area1",
            "area2 area3"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "1fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotlyOutput(outputId = "plot0"))
          ),
          grid_card(
            area = "area1",
            card_body(plotlyOutput(outputId = "plot1"))
          ),
          grid_card(
            area = "area2",
            card_body(plotOutput(outputId = "plot2"))
          ),
          grid_card(
            area = "area3",
            card_body(plotOutput(outputId = "plot3"))
          )
        )
      ),
      nav_panel(
        title = "Overview of Temperature",
        grid_container(
          layout = c(
            "area6 area0 area0 area0 area2 area2 area2",
            "area6 area0 area0 area0 area2 area2 area2",
            "area6 area3 area3 area3 area3 area3 area3",
            "area6 area4 area4 area5 area5 area7 area7"
          ),
          row_sizes = c(
            "0.81fr",
            "0.84fr",
            "1.19fr",
            "1.16fr"
          ),
          col_sizes = c(
            "1.21fr",
            "0.62fr",
            "1.19fr",
            "0.99fr",
            "0.99fr",
            "1fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotlyOutput(outputId = "plot4"))
          ),
          grid_card(
            area = "area6",
            full_screen = TRUE,
            card_header("Parameters"),
            card_body(
              selectInput(
                inputId = "SelectInputRD1",
                label = "Display Ridgeline Distribution of:",
                choices = list(
                  "Mean Temperature" = "meantemp",
                  "Max Temperature" = "maxtemp",
                  "Min Temperature" = "mintemp"
                ),
                selected = "meantemp"
              ),
              selectInput(
                inputId = "SelectInputCP1",
                label = "Display Cycle Plot of:",
                choices = list(
                  "Mean Temperature" = "meantemp",
                  "Max Temperature" = "maxtemp",
                  "Min Temperature" = "mintemp"
                ),
                selected = "meantemp"
              )
            )
          ),
          grid_card(
            area = "area2",
            card_body(plotlyOutput(outputId = "plot5"))
          ),
          grid_card(
            area = "area3",
            card_body(plotOutput(outputId = "plot6"))
          ),
          grid_card(
            area = "area4",
            card_body(plotOutput(outputId = "plot21"))
          ),
          grid_card(
            area = "area5",
            card_body(plotOutput(outputId = "plot22"))
          ),
          grid_card(
            area = "area7",
            card_body(plotOutput(outputId = "plot23"))
          )
        )
      ),
      nav_panel(
        title = "Correlation between weather variables",
        grid_container(
          layout = c(
            "area2 area0",
            ".     area1"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.47fr",
            "1.53fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotOutput(outputId = "plot7"))
          ),
          grid_card(
            area = "area1",
            card_body(plotOutput(outputId = "plot8"))
          ),
          grid_card(
            area = "area2",
            full_screen = TRUE,
            card_header("Parameters"),
            card_body(
              selectInput(
                inputId = "variable1",
                label = "Select Variable 1",
                choices = list(
                  "Rainfall" = "monthly_rainfall",
                  "Mean Temperature" = "mean_monthly_temperature",
                  "Max Temperature" = "max_monthly_temperature",
                  "Min Temperature" = "min_monthly_temperature"
                ),
                selected = "mean_monthly_temperature"
              ),
              selectInput(
                inputId = "variable2",
                label = "Select Variable 2",
                choices = list(
                  "Rainfall" = "monthly_rainfall",
                  "Mean Temperature" = "mean_monthly_temperature",
                  "Max Temperature" = "max_monthly_temperature",
                  "Min Temperature" = "min_monthly_temperature"
                ),
                selected = "monthly_rainfall"
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Weather by station",
        grid_container(
          layout = c(
            "area0 area0",
            "area1 area1"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "1.73fr",
            "0.27fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotlyOutput(outputId = "plot24"))
          ),
          grid_card(
            area = "area1",
            card_body(plotlyOutput(outputId = "plot25"))
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Confirmatory Data Analysis",
    tabsetPanel(
      nav_panel(
        title = "Significant changes in rainfall and temperature over the years?",
        grid_container(
          layout = c(
            "area4 area0 area1",
            "area5 area0 area1",
            "area6 area2 area3",
            "area7 area2 area3"
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.81fr",
            "0.99fr",
            "1.2fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotOutput(outputId = "plot9"))
          ),
          grid_card(
            area = "area1",
            card_body(plotOutput(outputId = "plot10"))
          ),
          grid_card(
            area = "area2",
            card_body(plotOutput(outputId = "plot11"))
          ),
          grid_card(
            area = "area3",
            card_body(plotOutput(outputId = "plot12"))
          ),
          grid_card(
            area = "area4",
            full_screen = TRUE,
            card_header("Top Left"),
            card_body(
              selectInput(
                inputId = "TestType1",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType1",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel1",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis1",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area5",
            full_screen = TRUE,
            card_header("Top Right"),
            card_body(
              selectInput(
                inputId = "TestType2",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType2",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel2",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis2",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area6",
            full_screen = TRUE,
            card_header("Bottom Left"),
            card_body(
              selectInput(
                inputId = "TestType3",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType3",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel3",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis3",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area7",
            full_screen = TRUE,
            card_header("Bottom Right"),
            card_body(
              selectInput(
                inputId = "TestType4",
                label = "Test Type",
                choices = list("choice a" = "a", "choice b" = "b")
              ),
              selectInput(
                inputId = "PlotType4",
                label = "Plot Type",
                choices = list("choice a" = "a", "choice b" = "b")
              ),
              selectInput(
                inputId = "conflevel4",
                label = "Confidence Level",
                choices = list("choice a" = "a", "choice b" = "b")
              ),
              selectInput(
                inputId = "PWDis4",
                label = "Pairwise Display",
                choices = list("choice a" = "a", "choice b" = "b")
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Significant differences in weather between months?",
        grid_container(
          layout = c(
            "area7 area0 area1",
            "area6 area0 area1",
            "area5 area2 area3",
            "area4 area2 area3"
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.74fr",
            "1.13fr",
            "1.13fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotOutput(outputId = "plot13"))
          ),
          grid_card(
            area = "area1",
            card_body(plotOutput(outputId = "plot14"))
          ),
          grid_card(
            area = "area2",
            card_body(plotOutput(outputId = "plot15"))
          ),
          grid_card(
            area = "area3",
            card_body(plotOutput(outputId = "plot16"))
          ),
          grid_card(
            area = "area4",
            full_screen = TRUE,
            card_header("Bottom Right"),
            card_body(
              selectInput(
                inputId = "TestType8",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType8",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel8",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis8",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area5",
            full_screen = TRUE,
            card_header("Bottom Left"),
            card_body(
              selectInput(
                inputId = "TestType7",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType7",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel7",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis7",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area6",
            full_screen = TRUE,
            card_header("Top Right"),
            card_body(
              selectInput(
                inputId = "TestType6",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType6",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel6",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis6",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area7",
            full_screen = TRUE,
            card_header("Top Left"),
            card_body(
              selectInput(
                inputId = "TestType5",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType5",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel5",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis5",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Significant differences in weather between locations?",
        grid_container(
          layout = c(
            "area7 area0 area1",
            "area6 area0 area1",
            "area5 area2 area3",
            "area4 area2 area3"
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.76fr",
            "1.12fr",
            "1.12fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotOutput(outputId = "plot17"))
          ),
          grid_card(
            area = "area1",
            card_body(plotOutput(outputId = "plot18"))
          ),
          grid_card(
            area = "area2",
            card_body(plotOutput(outputId = "plot19"))
          ),
          grid_card(
            area = "area3",
            card_body(plotOutput(outputId = "plot20"))
          ),
          grid_card(
            area = "area4",
            full_screen = TRUE,
            card_header("Bottom Right"),
            card_body(
              selectInput(
                inputId = "TestType12",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType12",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel12",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis12",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area5",
            full_screen = TRUE,
            card_header("Bottom Left"),
            card_body(
              selectInput(
                inputId = "TestType11",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType11",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel11",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis11",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area6",
            full_screen = TRUE,
            card_header("Top Right"),
            card_body(
              selectInput(
                inputId = "TestType10",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType10",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel10",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis10",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          ),
          grid_card(
            area = "area7",
            full_screen = TRUE,
            card_header("Top Left"),
            card_body(
              selectInput(
                inputId = "TestType9",
                label = "Test Type",
                choices = list(
                  "parametric" = "p",
                  "non-parametric" = "np",
                  "robust" = "robust",
                  "bayes" = "bayes"
                ),
                selected = "p"
              ),
              selectInput(
                inputId = "PlotType9",
                label = "Plot Type",
                choices = list(
                  "violin" = "violin",
                  "box" = "box",
                  "boxviolin" = "boxviolin"
                ),
                selected = "boxviolin"
              ),
              selectInput(
                inputId = "conflevel9",
                label = "Confidence Level",
                choices = list("95%" = "0.95", "99%" = "0.99"),
                selected = "0.95"
              ),
              selectInput(
                inputId = "PWDis9",
                label = "Pairwise Display",
                choices = list("significant" = "s", "non-significant" = "ns"),
                selected = "s"
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Weather data across stations for each year",
        grid_container(
          layout = c(
            "area4 area0 area1",
            ".     area2 area3"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.58fr",
            "1.21fr",
            "1.21fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotOutput(outputId = "plot26"))
          ),
          grid_card(
            area = "area1",
            card_body(plotOutput(outputId = "plot27"))
          ),
          grid_card(
            area = "area2",
            card_body(plotOutput(outputId = "plot28"))
          ),
          grid_card(
            area = "area3",
            card_body(plotOutput(outputId = "plot29"))
          ),
          grid_card(
            area = "area4",
            card_body(
              radioButtons(
                inputId = "RadioYear",
                label = "Select Year",
                choices = c("All",
                            "2014" = 2014,
                            "2015" = 2015,
                            "2016" = 2016,
                            "2017" = 2017,
                            "2018" = 2018,
                            "2019" = 2019,
                            "2020" = 2020,
                            "2021" = 2021,
                            "2022" = 2022,
                            "2023" = 2023)
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Detailed weather data for each station",
        grid_container(
          layout = c(
            "area2 area0 area0",
            ".     area1 area1"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.69fr",
            "1.31fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(plotOutput(outputId = "plot30"))
          ),
          grid_card(
            area = "area1",
            card_body(plotOutput(outputId = "plot31"))
          ),
          grid_card(
            area = "area2",
            card_body(
              selectInput(
                inputId = "SelectData",
                label = "Select Data",
                choices = list("choice a" = "a", "choice b" = "b")
              ),
              selectInput(
                inputId = "SelectWS",
                label = "Select a weather station",
                choices = list(
                  "Admiralty" = "Admiralty",
                  "Ang Mo Kio" = "Ang Mo Kio",
                  "Changi" = "Changi",
                  "Choa Chu Kang (South)" = "Choa Chu Kang (South)",
                  "Clementi" = "Clementi",
                  "East Coast Parkway" = "East Coast Parkway",
                  "Jurong Island" = "Jurong Island",
                  "Jurong (West)" = "Jurong (West)",
                  "Newton" = "Newton",
                  "Pasir Panjang" = "Pasir Panjang",
                  "Sentosa Island" = "Sentosa Island",
                  "Tai Seng" = "Tai Seng",
                  "Tuas South" = "Tuas South"
                ),
                selected = "Admiralty"
              )
            ),
            radioButtons(
              inputId = "RadioTimePeriod",
              label = "Select a time period",
              choices = list("Year" = "Year", "Month" = "Month"),
              width = "100%"
            )
          )
        )
      )
    )
  )
)

## Server

server <- function(input, output) {
  
## EDA - Overview of Rainfall
  
  output$plot0 <- renderPlotly({
    ggplot(rainfall_data_year,
                 aes(y=yearly_rainfall,
                     x = year))+
      geom_point()+
      geom_line() +
      labs(title="Rainfall from 2014 to 2023",
           y = "Rainfall volume (mm)",
           x = "Year") +
      scale_x_continuous(breaks =seq(2014,2023,1)) +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1),panel.spacing.y = unit(5,"lines"),legend.position = "none")
  })

  output$plot1 <- renderPlotly({
    ggplotly(ggplot(rainfall_data_month,
                    aes(y=monthly_rainfall,
                        x = as.factor(month),
                        fill = as.factor(year))) +
               geom_bar(stat = "identity")+
               facet_wrap(~year, scales = "free_x") +
               labs(title="Monthly rainfall each year from 2014 to 2023",
                    y = "Rainfall volume (mm)",
                    x = "Month") +
               theme_minimal()+
               theme(panel.spacing.y = unit(0.3, "lines"),text=element_text(size=10),
                     legend.position = "none")+
               scale_fill_discrete(name = "Year"))
  })
  
  output$plot2 <- renderPlot({
    ggplot(rainfall_data_month, 
                 aes(x = monthly_rainfall,
                     y = as.factor(year), 
                     fill = 0.5 - abs(0.5-stat(ecdf)))) +
      stat_density_ridges(geom = "density_ridges_gradient", 
                          calc_ecdf = TRUE) +
      scale_fill_viridis_c(name = "Tail probability",
                           direction = -1,
                           option="turbo")+
      theme_ridges()+
      labs(title="Distribution of Monthly Rainfall from 2014 to 2023",
           y="Year",
           x="Rainfall Volume (mm)")
  })

  output$plot3 <- renderPlot({
    ggplot() +
      geom_line(data = rainfall_data_month,
                aes(x = year,
                    y = monthly_rainfall,
                    group = month,
                    colour = as.factor(month)))+
      geom_hline(aes(yintercept=avgvalue),
                 data=hline.data,
                 linetype=6,
                 colour="red",
                 size=0.5)+
      facet_wrap(~month,scales = "free_x")+
      labs(title = "Rainfall by month from 2014 to 2023",
           colour = "Month") +
      xlab("Year")+
      ylab("Rainfall volume (mm)")+
      theme_tufte(base_family = "Helvetica")+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            legend.position = "none")
    
  })

  
  ## EDA - Overview of Temperature
  
  output$plot4 <- renderPlotly({
    
    p4 <- ggplot(combined_data, 
                 aes(x = year, 
                     y = value, 
                     color = temperature_type)) +
      geom_line() +
      labs(title = "Temperature Trends from 2014 to 2023",
           y = "Temperature (°C)",
           x = "Year",
           color="Temperature Type") +
      scale_x_continuous(breaks = seq(2014, 2023, 1)) +
      scale_color_manual(values = c("turquoise", "violetred2", "steelblue2"), 
                         labels = c("Mean", "Max", "Min")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    p4 <- ggplotly(p4, tooltip="all") %>%
      layout(legend = list(x = 0.6, y = 0.2))
    
    p4
  })
  
  output$plot5 <- renderPlotly({
    combined_data2 <- reshape2::melt(temp_month, id.vars = c("year", "month"), variable.name = "temperature_type")
    
    p17 <- ggplot(combined_data2, aes(x = month, 
                                      y = value,
                                      color = temperature_type)) +
      geom_line() +
      facet_wrap(~ year,scales = "free_x")+
      labs(title = "Detailed Temperature Trends from 2014 to 2023",
           y = "Temperature (°C)",
           x = "Month",
           color = "Temperature Type") +
      scale_x_continuous(breaks = seq(1,12, 1)) +
      scale_color_manual(values = c("turquoise", "violetred2", "steelblue2"), 
                         labels = c("Mean", "Max", "Min")) +
      theme_minimal() +
      theme(panel.border = element_rect(color = "lightgrey",linetype = "dashed", fill = NA, size = 1))
    
    p17 <- ggplotly(p17, tooltip = "all") %>%
      layout(legend = list(x = 0.6, y = 0.08))
    
    p17
  })
  
  output$plot6 <- renderPlot({

   ggplot(temp_month, 
                 aes_string(x = input$SelectInputRD1, 
                            y = "as.factor(year)", 
                     fill = "0.5 - abs(0.5-stat(ecdf))")) +
      stat_density_ridges(geom = "density_ridges_gradient", 
                          calc_ecdf = TRUE) +
      scale_fill_viridis_c(name = "Tail probability",
                           direction = -1,
                           option="turbo")+
      theme_ridges(font_size = 25)+
      scale_color_discrete(name = "Year") +
      labs(title=paste0("Distribution of ", input$SelectInputRD1," from 2014 to 2023"),
           y="Year",
           x="Temperature (°C)")

  })
  
  observeEvent(input$SelectInputCP1, {
    if (input$SelectInputCP1 == "meantemp") {
      output$plot21 <- renderPlot({
        
        ggplot() +
          geom_line(data = temp_month,
                    aes(x = as.factor(month),
                        y = meantemp,
                        group = year,
                        colour = as.factor(year)))+
          geom_hline(aes(yintercept=avgvalue),
                     data=hline_mean_temp.data,
                     linetype=6,
                     colour="red",
                     size=0.5)+
          facet_wrap(~year,scales = "free_x")+
          labs(axis.text.x=element_blank(),
               title = "Mean temperature by year from 2014 to 2023")+
          xlab("")+
          ylab("Degrees (°C)")+
          scale_color_discrete(name = "Year")+
          theme_tufte(base_family = "Helvetica",
                      base_size = 7)+
          theme(legend.position = "none") 
      })
      
      output$plot22 <- NULL
      output$plot23 <- NULL
      
    } else if (input$SelectInputCP1 == "maxtemp") {
      output$plot22 <- renderPlot({

        ggplot() +
          geom_line(data = temp_month,
                    aes(x = as.factor(month),
                        y = maxtemp,
                        group = year,
                        colour = as.factor(year)))+
          geom_hline(aes(yintercept=avgvalue),
                     data=hline_max_temp.data,
                     linetype=6,
                     colour="red",
                     size=0.5)+
          facet_wrap(~year,scales = "free_x")+
          labs(axis.text.x=element_blank(),
               title = "Max temperature by year from 2014 to 2023")+
          xlab("")+
          ylab("Degrees (°C)")+
          scale_color_discrete(name = "Year")+
          theme_tufte(base_family = "Helvetica",
                      base_size = 7)+
          theme(legend.position = "none")
        
      })
      
      output$plot21 <- NULL
      output$plot23 <- NULL
      
    } else if (input$SelectInputCP1 == "mintemp") {
      output$plot23 <- renderPlot({
        ggplot() +
          geom_line(data = temp_month,
                    aes(x = as.factor(month),
                        y = mintemp,
                        group = year,
                        colour = as.factor(year)))+
          geom_hline(aes(yintercept=avgvalue),
                     data=hline_min_temp.data,
                     linetype=6,
                     colour="red",
                     size=0.5)+
          facet_wrap(~year,scales = "free_x")+
          labs(axis.text.x=element_blank(),
               title = "Min temperature by year from 2014 to 2023")+
          xlab("")+
          ylab("Degrees (°C)")+
          scale_color_discrete(name = "Year")+
          theme_tufte(base_family = "Helvetica",
                      base_size = 7)+
          theme(legend.position = "none")
        
      })
      
      output$plot21 <- NULL
      output$plot22 <- NULL
      
    }
  })
  
  ## EDA - Correlation between variables

  output$plot7 <- renderPlot({
    ggscatterstats(
      data = weather_data_imputed,
      x = !!input$variable1,
      y = !!input$variable2,
      xlab = "Variable 1", ## label for the x-axis
      ylab = "Variable 2", ## label for the y-axis
      title = "Correlation Scatter Plot",
      marginal = FALSE
    )
  })
  
  output$plot8 <- renderPlot({
    ggcorrmat(
      weather_data_imputed,
      cor.vars = c(min_monthly_temperature, mean_monthly_temperature, max_monthly_temperature, monthly_rainfall),
      cor.vars.names = NULL,
      matrix.type = "upper",
      type = "parametric",
      tr = 0.1,
      partial = TRUE,
      digits = 2L,
      sig.level = 0.05,
      conf.level = 0.95,
      bf.prior = 0.707,
      p.adjust.method = "holm",
      pch = "cross",
      ggcorrplot.args = list(method = "square", outline.color = "black", pch.cex = 14),
      package = "RColorBrewer",
      palette = "Dark2",
      colors = c("#E69F00", "white", "#009E73"),
      ggtheme = ggstatsplot::theme_ggstatsplot(),
      ggplot.component = NULL,
      title = "Correlation Matrix" ,
      subtitle = NULL,
      caption = NULL
    )
  })
  
  ## EDA - Weather by Station
  
  output$plot24 <- renderPlotly({
    
    ggplotly(ggplot(rainfall_data_stn,
                  aes(y=yearly_rainfall,
                      x = year,
                      group = station,
                      color = station)) +
      geom_line() +
      facet_wrap(~station,scales = "free_x") +
      labs(title="Yearly rainfall across weather stations from 2014 to 2023",
           y = "Rainfall volume (mm)",
           x = "Year") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90,hjust=1),
            panel.spacing.y = unit(0.05,"lines"),
            legend.position = "none")) %>%
      layout(width = 600, height = 700)
    
  })
  
  output$plot25 <- renderPlotly({
    
    p29 <- ggplot(combined_data3, aes(x = year, 
                                      y = value,
                                      color = temperature_type)) +
      geom_line() +
      facet_wrap(~ station,scales = "free_x")+
      labs(title = "Detailed Temperature Trends across stations from 2014 to 2023",
           y = "Temperature (°C)",
           x = "Year",
           color = "Temperature Type") +
      scale_x_continuous(breaks = seq(2014,2023, 1)) +
      scale_color_manual(values = c("turquoise", "violetred2", "steelblue2"), 
                         labels = c("Mean", "Max", "Min")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            panel.border = element_rect(color = "lightgrey",linetype = "dashed", fill = NA, size = 1))
    
    p29 <- ggplotly(p29, tooltip = "all",width = 800, height = 600)%>%
      layout(legend = list(x = 0.7, y = 0))
    
    p29
  })
  
  ## CDA 1 - Are the changes in rainfall and temperature over the years statistically significant?
  
  output$plot9 <- renderPlot({
    
    p3 <- ggbetweenstats(
      data = rainfall_data_month,
      x = year, 
      y = monthly_rainfall,
      type = input$TestType1,
      pairwise.display = input$PWDis1,
      conf.level = input$conflevel1,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Rainfall across 10 years (2014 to 2023)",
      ylab = "Rainfall volume (mm)",
      xlab = "Year",
      ggsignif.args = list(textsize = 5)
    ) +
      theme(text = element_text(size = 10), 
            plot.title=element_text(size=10),
            axis.text.x = element_text(angle =90))
    
    p3
    
  })
  
  output$plot10 <- renderPlot({
    
    p8 <- ggbetweenstats(
      data = temp_month,
      x = year, 
      y = meantemp,
      type = input$TestType2,
      pairwise.display = input$PWDis2,
      conf.level = input$conflevel2,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Median Mean Temperature of each year across 10 years (2014 to 2023)",
      ylab = "Temperature (°C)",
      xlab = "Year",
      ggsignif.args = list(textsize = 5)
    ) +
      theme(text = element_text(size = 10),
            plot.title=element_text(size=10),
            axis.text.x = element_text(angle =90))
    p8
    
  })
  
  output$plot11 <- renderPlot({
    
    p9 <- ggbetweenstats(
      data = temp_month,
      x = year, 
      y = maxtemp,
      type = input$TestType3,
      pairwise.display = input$PWDis3,
      conf.level = input$conflevel3,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Median Highest Max Temperature of each year across 10 years (2014 to 2023)",
      ylab = "Temperature (°C)",
      xlab = "Year",
      ggsignif.args = list(textsize = 5)
    ) +
      theme(text = element_text(size = 10),
            plot.title=element_text(size=10),
            axis.text.x = element_text(angle =90))
    p9
    
  })
  
  output$plot12 <- renderPlot({
    
    p10 <- ggbetweenstats(
      data = temp_month,
      x = year, 
      y = mintemp,
      type = input$TestType4,
      pairwise.display = input$PWDis4,
      conf.level = input$conflevel4,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Median Lowest Min Temperature of each year across 10 years (2014 to 2023)",
      ylab = "Temperature (°C)",
      xlab = "Year",
      ggsignif.args = list(textsize = 5)
    ) +
      theme(text = element_text(size = 10),
            plot.title=element_text(size=10),
            axis.text.x = element_text(angle =90))
    p10
    
  })
  
  ## CDA 2 - Are there really certain months “drier” or “wetter”/ “hotter” or “cooler”?
  
  output$plot13 <- renderPlot({
    
    p13 <- ggbetweenstats(
      data = rainfall_data_month,
      x = month, 
      y = monthly_rainfall,
      type = input$TestType5,
      pairwise.display = input$PWDis5,
      conf.level = input$conflevel5,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Median Rainfall across months (2014 to 2023)",
      ylab = "Rainfall volume (mm)",
      xlab = "Month",
      ggsignif.args = list(textsize = 5)
    ) +
      theme(text = element_text(size = 10), plot.title=element_text(size=10))
    p13
    
  })
  
  output$plot14 <- renderPlot({
    
    p21 <- ggbetweenstats(data = temp_month,
                          x = month,
                          y = meantemp,
                          type = input$TestType6,
                          pairwise.display = input$PWDis6,
                          conf.level = input$conflevel6,
                          results.subtitle = TRUE,
                          messages = FALSE,
                          title = "Distribution of Mean Temperature by month (2014 to 2023)",
                          ylab = "Temperature (°C)",
                          xlab = "Month",
                          ggsignif.args = list(textsize =5)) +
      theme(text = element_text(size = 10),plot.title = element_text(size = 10))
    p21
    
  })
  
  output$plot15 <- renderPlot({
    
    p22 <- ggbetweenstats(data = temp_month,
                          x = month,
                          y = maxtemp,
                          type = input$TestType7,
                          pairwise.display = input$PWDis7,
                          conf.level = input$conflevel7,
                          results.subtitle = TRUE,
                          messages = FALSE,
                          title = "Distribution of Median Highest Max Temperature by month (2014 to 2023)",
                          ylab = "Temperature (°C)",
                          xlab = "Month",
                          ggsignif.args = list(textsize =5)) +
      theme(text = element_text(size = 10),plot.title = element_text(size = 10))
    p22
    
  })
  
  output$plot16 <- renderPlot({
    
    p23 <- ggbetweenstats(data = temp_month,
                          x = month,
                          y = mintemp,
                          type = input$TestType8,
                          pairwise.display = input$PWDis8,
                          conf.level = input$conflevel8,
                          results.subtitle = TRUE,
                          messages = FALSE,
                          title = "Distribution of Median Lowest Min Temperature by month (2014 to 2023)",
                          ylab = "Temperature (°C)",
                          xlab = "Month",
                          ggsignif.args = list(textsize =5)) +
      theme(text = element_text(size = 10),plot.title = element_text(size = 10))
    p23
    
  })
  
  ## CDA 3 - Are there certain locations “drier” or “wetter”/ “hotter” or “cooler”?
  
  output$plot17 <- renderPlot({
    
    p25 <- ggbetweenstats(
      data = rainfall_data_stn,
      x = station, 
      y = yearly_rainfall,
      type = input$TestType9,
      pairwise.display = input$PWDis9,
      conf.level = input$conflevel9,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Yearly Rainfall by Station",
      ylab = "Rainfall volume (mm)",
      xlab = "Station",
      ggsignif.args = list(textsize = 3)
    ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
            plot.title = element_text(size = 10),
            text = element_text(size = 10))
    p25
    
  })
  
  output$plot18 <- renderPlot({
    
    p30 <- ggbetweenstats(
      data = temp_stn,
      x = station, 
      y = meantemp,
      type = input$TestType10,
      pairwise.display = input$PWDis10,
      conf.level = input$conflevel10,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Mean Temperature by Station",
      ylab = "Temperature (°C)",
      xlab = "Station",
      ggsignif.args = list(textsize = 3)
    ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
            plot.title = element_text(size = 10),
            text = element_text(size = 10))
    p30
    
  })
  
  output$plot19 <- renderPlot({
    
    p31 <- ggbetweenstats(
      data = temp_stn,
      x = station, 
      y = maxtemp,
      type = input$TestType11,
      pairwise.display = input$PWDis11,
      conf.level = input$conflevel11,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Max Temperature by Station",
      ylab = "Temperature (°C)",
      xlab = "Station",
      ggsignif.args = list(textsize = 3)
    ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
            plot.title = element_text(size = 10),
            text = element_text(size = 10))
    p31
    
  })
  
  output$plot20 <- renderPlot({
    
    p32 <- ggbetweenstats(
      data = temp_stn,
      x = station, 
      y = mintemp,
      type = input$TestType12,
      pairwise.display = input$PWDis12,
      conf.level = input$conflevel12,
      results.subtitle = TRUE,
      messages = FALSE,
      title="Distribution of Min Temperature by Station",
      ylab = "Temperature (°C)",
      xlab = "Station",
      ggsignif.args = list(textsize = 3)
    ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
            plot.title = element_text(size = 10),
            text = element_text(size = 10))
    p32
    
  })
  
  ## CDA 3.1 – Detailed breakdown of monthly mean temp and monthly rainfall across stations for each year
  
  ### Distribution Analysis Panel
  
  ## Top Left Plot
  output$plot26 <- renderPlot({
    data <- weather_data_imputed %>%
      mutate(Year = year(tdate), Month = month(tdate))
    
    data <- data %>%
      filter(!!input$selectyear == "All" | Year == !!input$selectyear)
    
    data$station <- reorder(data$station, data$mean_monthly_temperature)
    
    ggbetweenstats(
      data = data,
      x = station, 
      y = mean_monthly_temperature,
      type = "p",
      mean.ci = TRUE, 
      title = "Mean Monthly Temperature",
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE) +
      labs(title = 'Violin Plot of Mean Monthly Temperature by Stations',
           y = "Temperature") +
      theme(axis.text.x = element_text(angle = 60,
                                       size = 8))
  })
  
  ## Bottom Left Plot
  
  output$plot27 <- renderPlot({
    data <- weather_data_imputed %>%
      mutate(Year = year(tdate), Month = month(tdate))
    
    data <- data %>%
      filter(!!input$selectyear == "All" | Year == !!input$selectyear)
    
    data$station <- reorder(data$station, data$mean_monthly_temperature)
    
    ggplot(data,
           aes(x = mean_monthly_temperature, 
               y = station, 
               fill = stat(x))) +
      geom_density_ridges_gradient(scale =2,
                                   rel_min_height = 0.01,
                                   gradient_lwd = 1.) +
      scale_y_discrete(name= NULL) +
      scale_fill_viridis_c(name = "°C", option = "C") +
      labs(title = 'Ridgeline Plot of Mean Monthly Temperature by Stations',
           x = "Temperature (°C)",
           y = "Station") +
      theme_ridges(font_size = 10, grid = TRUE) +
      theme(plot.title = element_text(size = 14),
            plot.subtitle = element_text(size = 10),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8, angle = 360))
  })
  
  ## Top Right Plot
  output$plot28 <- renderPlot({
    data <- weather_data_imputed %>%
      mutate(Year = year(tdate), Month = month(tdate))
    
    data <- data %>%
      filter(!!input$selectyear == "All" | Year == !!input$selectyear)
    
    data$station <- reorder(data$station, data$monthly_rainfall)
    
    ggbetweenstats(
      data = data,
      x = station, 
      y = monthly_rainfall,
      type = "p",
      mean.ci = TRUE, 
      title = "Monthly Temperature",
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE) +
      labs(title = 'Violin Plot of Monthly Rainfall by Stations',
           y = "Temperature") +
      theme(axis.text.x = element_text(angle = 60,
                                       size = 8))
  })
  
  ## Bottom Right Plot
  output$plot29 <- renderPlot({
    data <- weather_data_imputed %>%
      mutate(Year = year(tdate), Month = month(tdate))
    
    data <- data %>%
      filter(!!input$selectyear == "All" | Year == !!input$selectyear)
    
    data$station <- reorder(data$station, data$monthly_rainfall)
    
    ggplot(data,
           aes(x = monthly_rainfall, 
               y = station, 
               fill = stat(x))) +
      geom_density_ridges_gradient(scale =2,
                                   rel_min_height = 0.01,
                                   gradient_lwd = 1.) +
      scale_y_discrete(name= NULL) +
      scale_fill_viridis_c(name = "°C", option = "C") +
      labs(title = 'Ridgeline Plot of Monthly Rainfall by Stations',
           x = "Temperature (°C)",
           y = "Station") +
      theme_ridges(font_size = 10, grid = TRUE) +
      theme(plot.title = element_text(size = 14),
            plot.subtitle = element_text(size = 10),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8, angle = 360))
  })
  
  ## CDA 3.2 – Detailed breakdown of yearly and monthly data for each station


  ### Means Comparison Panel
  
  output$plot30 <- renderPlot({
    data <- weather_data_imputed %>%
      mutate(Year = year(tdate), Month = month(tdate))
    
    ggbetweenstats(
      data = data,
      x = !!input$time_display, 
      y = !!input$variable5,
      type = "p",
      mean.ci = TRUE, 
      title = "Violine Plots by All Stations",
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
  })
  
  output$plot31 <- renderPlot({
    data <- weather_data_imputed %>%
      mutate(Year = year(tdate), Month = month(tdate))
    
    data1 <- weather_data_imputed %>%
      filter(station == !!input$shstation2)
    
    ggbetweenstats(
      data = data1,
      x = !!input$time_display, 
      y = !!input$variable5,
      type = "p",
      mean.ci = TRUE, 
      title = "Violine Plots by Selected Station",
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
    
  })

}

shinyApp(ui, server)
