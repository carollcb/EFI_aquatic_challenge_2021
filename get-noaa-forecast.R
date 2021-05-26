# remotes::install_github("eco4cast/neon4cast")

library(neon4cast)
library(tidyverse)

save_daily_forecast <- function(siteID, date, my_dir, my_filename){
  download_noaa(siteID, date, my_dir)
  stack_df <- stack_noaa(my_dir) %>%
    mutate(date = lubridate::as_date(time)) %>%
    group_by(date, ensemble) %>%
    summarize(air_temperature = mean(air_temperature),
              air_pressure = mean(air_pressure),
              Ed_long = mean(surface_downwelling_longwave_flux_in_air),
              Ed_short = mean(surface_downwelling_shortwave_flux_in_air),
              precip = mean(precipitation_flux),
              specific_humidity = mean(specific_humidity),
              cloud_area = mean(cloud_area_fraction),
              wind_speed = mean(wind_speed))
  stack_df %>% write_csv(filename)
  message(glue('saved forecast for {siteID} starting {date} as {my_filename}'))
  return(stack_df)
}

save_daily_forecast('POSE', date = '2021-05-01', my_dir = 'noaa_POSE', my_filename = 'noaa-POSE-2021-05-01.csv')
save_daily_forecast('BARC', date = '2021-05-01', my_dir = 'noaa_POSE', my_filename = 'noaa-BARC-2021-05-01.csv')

