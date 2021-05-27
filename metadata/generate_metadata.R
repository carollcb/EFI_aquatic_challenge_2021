library(neon4cast)
# theme_name-year-month-day-team_name_ID.xml

test_forecast <- readr::read_csv("https://data.ecoforecast.org/forecasts/aquatics/aquatics-2021-03-01-EFInull.csv.gz")
test_forecast %>% write_csv('metadata/aquatics-test-forecast.csv')

# only run if YML needs to be recreated entirely
# create_model_metadata(forecast_file = my_forecast_file)

# To create XML (despite name of fxn)
# for BARC lake model
write_metadata_eml(forecast_file = 'metadata/aquatics-test-forecast.csv',
                   metadata_yaml = 'metadata/aquatics-2021-05-01-beyondBARC.yml',
                   forecast_issue_time = Sys.Date(),
                   forecast_iteration_id = '1')
# for POSE stream model
write_metadata_eml(forecast_file = 'metadata/aquatics-test-forecast.csv',
                   metadata_yaml = 'metadata/aquatics-2021-05-01-beyondPOSE.yml',
                   forecast_issue_time = Sys.Date(),
                   forecast_iteration_id = '1')
