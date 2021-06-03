library(tidyverse)
library(neon4cast)

test_forecast <- read_csv('metadata/aquatics-2021-05-01-BBTW-TEST.csv')

barc_forecast_file <- 'aquatics-2021-05-01-BBTW.csv'
barc_forecast <- read_csv(barc_forecast_file)

barc_forecast2 <- barc_forecast %>%
  mutate(ensemble1 = gsub("ens", 1, ensemble)) %>%
  mutate(ensemble2 = str_extract(ensemble1, "^(.*)_")) %>%
  mutate(ensemble2 = gsub("_", "", ensemble2)) %>%
  mutate(ensemble3 = str_extract(ensemble1, "\\_(.*)")) %>%
  mutate(ensemble3 = gsub("_", "", ensemble3)) %>%
  mutate(ensemble3 = str_pad(ensemble3, 3, 'left', 0)) %>%
  mutate(ensemble = glue::glue('{ensemble2}{ensemble3}')) %>%
  mutate(siteID = 'BARC',
         forecast = 1,
         data_assimilation = 0) %>%
  dplyr::select(time, siteID, ensemble, oxygen, temperature, forecast, data_assimilation)

unique(barc_forecast2$ensemble) %>% sort()
head(barc_forecast2)

barc_forecast2 %>% write_csv("forecast-submissions/forecast1/aquatics-2021-05-01-BBTW.csv")

pose_forecast_file <- 'aquatics-2021-05-01-BTW.csv'
pose_forecast <- read_csv(pose_forecast_file)
# unique(pose_forecast$ensemble)
pose_forecast2 <- pose_forecast %>%
  mutate(ensemble = gsub("ens", "", ensemble)) %>%
  mutate(ensemble = as.numeric(ensemble) + 1) %>%
  mutate(forecast = 1, data_assimilation = 0) %>%
  mutate(oxygen = NA) %>%
  dplyr::select(time, siteID, ensemble, oxygen, temperature, forecast, data_assimilation)

pose_forecast2 %>% write_csv("forecast-submissions/forecast1/aquatics-2021-05-01-BTW.csv")

# use generate-metadata.R to create new XML from YML and CSV
neon4cast::submit(forecast_file = 'forecast-submissions/forecast1/aquatics-2021-05-01-BBTW.csv',
                  metadata = 'forecast-submissions/forecast1/aquatics-2021-05-01-BBTW.xml')

neon4cast::submit(forecast_file = 'forecast-submissions/forecast1/aquatics-2021-05-01-BTW.csv',
                  metadata = 'forecast-submissions/forecast1/aquatics-2021-05-01-BTW.xml')
