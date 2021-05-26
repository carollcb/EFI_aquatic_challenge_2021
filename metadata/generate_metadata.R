library(neon4cast)
# theme_name-year-month-day-team_name_ID.xml

test_forecast <- readr::read_csv("https://data.ecoforecast.org/forecasts/aquatics/aquatics-2021-03-01-EFInull.csv.gz")
test_forecast %>% write_csv('metadata/aquatics-test-forecast.csv')

my_forecast_file <- 'metadata/aquatics-test-forecast.csv'

create_model_metadata(forecast_file = my_forecast_file)

write_metadata_eml(forecast_file = 'aquatics-2021-05-21-name10char.csv',
                   metadata_yaml = 'aquatics-2021-05-21-name10char.yml',
                   forecast_issue_time = Sys.Date(),
                   forecast_iteration_id = '1')
