pacman::p_load(tidyverse, lubridate)

weatherdata <-read_rds("data/weather_data_imputed.rds")

weatherdata_cda <- weatherdata %>%
  mutate(MONTH = month(tdate),
         YEAR = year(tdate))