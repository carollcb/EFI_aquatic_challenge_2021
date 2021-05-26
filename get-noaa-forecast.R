# remotes::install_github("eco4cast/neon4cast")

library(neon4cast)
library(tidyverse)

download_noaa('POSE', date = '2021-05-01', dir = 'noaa_POSE')
noaa_pose <- stack_noaa('noaa_POSE')
noaa_pose %>% write_csv('noaa-POSE-2021-05-01.csv')

download_noaa('BARC', date = '2021-05-01', dir = 'noaa_BARC')
noaa_barc <- stack_noaa('noaa_BARC')
noaa_barc %>% write_csv('noaa-BARC-2021-05-01.csv')
