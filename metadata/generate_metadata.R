library(neon4cast)
# theme_name-year-month-day-team_name_ID.xml

test_forecast <- readr::read_csv("https://data.ecoforecast.org/forecasts/aquatics/aquatics-2021-03-01-EFInull.csv.gz")
test_forecast %>% write_csv('metadata/aquatics-test-forecast.csv')

# only run if YML needs to be recreated entirely
# create_model_metadata(forecast_file = my_forecast_file)

# To create XML (despite name of fxn)
# for BARC lake model (BBTW)
write_metadata_eml(forecast_file = 'metadata/aquatics-2021-05-01-BBTW.csv',
                   metadata_yaml = 'metadata/aquatics-2021-05-01-BBTW.yml',
                   forecast_issue_time = Sys.Date(),
                   forecast_iteration_id = '1')
# for POSE stream model (BTW)
write_metadata_eml(forecast_file = 'metadata/aquatics-2021-05-01-BTW.csv',
                   metadata_yaml = 'metadata/aquatics-2021-05-01-BTW.yml',
                   forecast_issue_time = Sys.Date(),
                   forecast_iteration_id = '1')

# Where year, month, and day are the year, month, and day for the first day of
# the submitted forecast and the team_name_ID is the code for the team name
# that is specified in the registration (team_name_ID is a 10 character name with no spaces in it).
