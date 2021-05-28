rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(gridExtra)
library(ggplot2)
library(pracma)
library(tidyverse)
library(RColorBrewer)
library(zoo)
library(deSolve)
library(LakeMetabolizer)

library(dplyr)
library(readr)
library(LakeEnsemblR)
library(gotmtools)
library(lubridate)

library(thermod)

### GETTING CONFIGURATION INPUT FROM LER YAML FILE
config_file <- 'LakeEnsemblR.yaml'
folder = '.'
parameters <- configure_from_ler(config_file <- config_file, folder = folder, mode = 'forecast')

# load in the boundary data
bound <- read_delim(paste0(folder,'/meteo.txt'), delim = '\t')

colnames(bound) <- c('Day','Jsw','Tair','Dew','vW')

# function to calculate wind shear stress (and transforming wind speed from km/h to m/s)
bound$Uw <- 19.0 + 0.95 * (bound$vW * 1000/3600)^2
bound$vW <- bound$vW * 1000/3600

boundary <- bound

ensemble_data <- read_delim(paste0(folder, '/../noaa-BARC-2021-05-01_daily.csv'), delim = ',')
ensemble_bound = data.frame('Day' = max(bound$Day) + (match(ensemble_data$date,unique(ensemble_data$date))),
                            'Jsw' = ensemble_data$Ed_short * 2.0E-5 *3600*24,
                            'Tair' = ensemble_data$air_temperature -273.15,
                            'Dew' = ( ensemble_data$air_temperature -273.15)  - ((100 - 100 * ensemble_data$relative_humidity)/5.),
                            'vW' = ensemble_data$wind_speed * 1000/3600,
                            'Uw' =  19.0 + 0.95 * (ensemble_data$wind_speed * 1000/3600)^2,
                            'ensemble' = ensemble_data$ensemble)
head(ensemble_bound)
# simulation maximum length
# times <- seq(from = 1, to = max(boundary$Day), by = 1)

start_date <- get_yaml_value(config_file, "time", "start")
stop_date <- get_yaml_value(config_file, "time", "stop")
time_seq =seq(as.Date(start_date), as.Date(stop_date), by = 'day')

if (file.exists('output.txt')){
  file.remove('output.txt')
}

wq_parameters <- append(parameters, c(0.001 / 10000,
                                      100, 15000 * 1e4, 100))
parameters[19] = 3.2 # calibration parameter
wq_parameters[19] = parameters[19] # calibration parameter

# initial water temperatures & do
yini <- c(18, 10 * 1000/1e6  * wq_parameters[1])

ice_on = TRUE # ice "simulation" on or off?

obs <-read_delim(paste0('obs_forecast.txt'), delim = ',')
obs.df <- data.frame('Time' = as.Date(obs$Date),
                     'WT_obs' = obs$Water_Temperature_celsius,
                     'DO_obs' = obs$Dissolved_Oxygen_milliGramPerLiter)

result_ensemble <- c()
for (ens_run in unique(ensemble_bound$ensemble)){
  name_run <- paste0('em_',ens_run)
  print(name_run)

  ensemble_driver = rbind(boundary, subset(ensemble_bound, ensemble == ens_run)[1:15,-c(ncol(ensemble_bound))])

  times <- seq(from = 1, to = max(ensemble_driver$Day), by = 1)

  bc = ensemble_driver
  params = wq_parameters
  ini = yini
  times = times
  ice = ice_on
  observed = obs.df[match(time_seq, obs.df$Time),]

  out <- run_temp_oxygen_forecast(bc = ensemble_driver, params = wq_parameters, ini = yini, times = times, ice = ice_on,
                                  observed = obs.df[match(time_seq, obs.df$Time),])

  out.df <- c()
  for (j in 1:length(out)){
    if (j == 1){
      out.df <- unlist(out[[match(j, seq(1, length(out)))]])
    } else {
      out.df <- rbind(out.df, unlist(out[[match(j, seq(1, length(out)))]]))
    }
  }

  result <- data.frame('Time' = rep(time_seq, length(out)),
                       'WT_sim' = out.df[,2],
                       'DO_sim' = out.df[,3],
                       'run' = paste0(ens_run,'_', rep(seq(1,100),each =length(time_seq)))
                       )

  # result <- data.frame('Time' = time_seq,
  #                      'WT_sim' = out[,2],
  #                      'DO_sim' = out[,3],
  #                      'run' = rep('1', length(time_seq)))
  head(result)
  result_filter = result %>%
    dplyr::group_by(run) %>%
    mutate(WT_sim_kf = kalman_filtering(time = Time, series = WT_sim),
           DO_sim_kf = kalman_filtering(time = Time, series = DO_sim))
  # result_filter$WT_sim = kalman_filtering(time = result_filter$Time, series = result_filter$WT_sim)
  # result_filter$DO_sim = kalman_filtering(time = result_filter$Time, series = result_filter$DO_sim)
  head(result_filter)
  result_filter$ensemble = name_run

  result_ensemble <- rbind(result_ensemble, result_filter)
}

save(result_ensemble, file = 'result_ensemble.RData')

forecast_period <- c("2021-05-01","2021-05-02","2021-05-03","2021-05-04","2021-05-05","2021-05-06","2021-05-07",
                     "2021-05-08")
result_ensemble %>%
  dplyr::filter(Time >= min(forecast_period) & Time <= max(forecast_period)) %>%
  dplyr::group_by(Time) %>%
  summarise(mean_wtr = mean(WT_sim_kf), sd_wtr = sd(WT_sim_kf),
         mean_do = mean(DO_sim_kf), sd_do = sd(DO_sim_kf)) %>%
  select(Time, mean_wtr, sd_wtr, mean_do, sd_do)

result_ensemble_df = result_ensemble
result_ensemble_df$ensemble = NULL
forecast_csv <- result_ensemble_df %>%
  dplyr::filter(Time >= min(forecast_period) & Time <= max(forecast_period)) %>%
  # dplyr::group_by(Time, run) %>%
  dplyr::mutate('DO_sim_kf_conv' = DO_sim_kf / 1000 /  wq_parameters[1] * 1e6,
                'siteID' = 'BARCO') %>%
  dplyr::rename('time' = Time, 'ensemble' = run,
                'oxygen' = DO_sim_kf_conv, 'temperature' = WT_sim_kf) %>%
  select(time, ensemble, siteID, oxygen, temperature)
head(forecast_csv)

write.csv(x = forecast_csv, file = 'aquatics-2021-05-01-BBTW.csv', quote = F, row.names = F)

# df <- merge(result_filter, obs.df, by = 'Time', all = TRUE)
# head(df)

g1 <- ggplot(result_ensemble) +
  geom_line(aes(x=Time, y=WT_sim_kf, col=as.factor(run),
                group = as.factor(run))) +
  geom_point(data = obs.df, aes(x=Time, y=WT_obs, col='Observed'), col = 'blue') +
  labs(x = 'Simulated Time', y = 'WT in deg C')  +
  theme_bw()+
  xlim(as.Date('2020-01-01'), as.Date('2021-05-15')) +
  geom_vline(xintercept= as.Date('2021-05-01'),linetype=4) +
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="none");g1
ggsave(file='BARCO_thermod_forecast_wtemp.png', g1, dpi = 300,width = 200,height = 250, units = 'mm')

g2 <- ggplot(result_ensemble) +
  geom_line(aes(x=Time, y=DO_sim_kf/ 1000 /  wq_parameters[1] * 1e6,  col=as.factor(run),
                group = as.factor(run))) +
  geom_point(data = obs.df, aes(x=Time, y=DO_obs, col='Observed'), col = 'blue') +
  labs(x = 'Simulated Time', y = 'DO in g/m3')  +
  theme_bw()+
  xlim(as.Date('2020-01-01'), as.Date('2021-05-15')) +
  geom_vline(xintercept= as.Date('2021-05-01'),linetype=4) +
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="none");g2
ggsave(file='BARCO_thermod_forecast_do.png', g2, dpi = 300,width = 200,height = 250, units = 'mm')

g3 <- ggplot(result_ensemble) +
  geom_line(aes(x=Time, y=WT_sim_kf, col=as.factor(run),
                group = as.factor(run))) +
  geom_point(data = obs.df, aes(x=Time, y=WT_obs, col='Observed'), col = 'blue') +
  labs(x = 'Simulated Time', y = 'WT in deg C')  +
  theme_bw()+
  xlim(as.Date('2021-04-30'), as.Date('2021-05-15')) +
  geom_vline(xintercept= as.Date('2021-05-01'),linetype=4) +
  ylim(15,20)+
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="none");g1
ggsave(file='BARCO_thermod_forecast_period_wtemp.png', g3, dpi = 300,width = 250,height = 200, units = 'mm')

g4 <- ggplot(result_ensemble) +
  geom_line(aes(x=Time, y=DO_sim_kf/ 1000 /  wq_parameters[1] * 1e6,  col=as.factor(run),
                group = as.factor(run))) +
  geom_point(data = obs.df, aes(x=Time, y=DO_obs, col='Observed'), col = 'blue') +
  labs(x = 'Simulated Time', y = 'DO in g/m3')  +
  theme_bw()+
  xlim(as.Date('2021-04-30'), as.Date('2021-05-15')) +
  geom_vline(xintercept= as.Date('2021-05-01'),linetype=4) +
  ylim(7,9.5) +
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="none");g4
ggsave(file='BARCO_thermod_forecast_period_do.png', g4, dpi = 300,width = 250,height = 200, units = 'mm')

#
#
# output <- read.table('output.txt')
# #write.table(matrix(c(sw, lw, water_lw, conv, evap, Rh, t, ice_param,ATM, NEP), nrow=1),
#
# output <- data.frame('sw'=output[,1],'lw'=output[,2],'water_lw'=output[,3],'conv'=output[,4],
#                      'evap'=output[,5], 'Rh' = output[,6],'time' = output[,7],
#                      'ice' = output[8], 'atmosp_exchange' = output[,9],
#                      'NEP' = output[,10])
#
# output$balance <- apply(output[,c(1:6)],1, sum)
#
# g3 <- ggplot(output) +
#   geom_line(aes(x = time,y = sw, col = 'Shortwave')) +
#   geom_line(aes(x = time,y = lw, col = 'Longwave')) +
#   geom_line(aes(x = time,y = water_lw, col = 'Reflection')) +
#   geom_line(aes(x = time,y = conv, col = 'Conduction')) +
#   geom_line(aes(x = time,y = evap, col = 'Evaporation')) +
#   geom_line(aes(x = time,y = balance, col = 'Sum'), linetype = "dashed") +
#   scale_colour_brewer("Energy terms", palette="Set3") +
#   labs(x = 'Simulated Time', y = 'Fluxes in cal/(cm2 d)')  +
#   theme_bw()+
#   theme(legend.position="bottom");g3
#
# g4 <- ggplot(output) +
#   geom_line(aes(x = time,y = atmosp_exchange, col = 'Atmosphere')) +
#   geom_line(aes(x = time,y = NEP, col = 'Productivity')) +
#   scale_colour_brewer("Energy terms", palette="Set3") +
#   labs(x = 'Simulated Time', y = 'Fluxes in mg/(cm3 d)')  +
#   theme_bw()+
#   theme(legend.position="bottom");g4



### forecasting framework
#
# for (ii in 1:3){
#   print(paste0('run number: ', ii))
#
#   boundary_noise <- add_noise(boundary)
#
#   for (jj in 1:100){
#     y_noise <- yini
#     y_noise[1] <- rnorm(1, mean = y_noise[1], sd = 0.1)
#     y_noise[2] <- rnorm(1, mean = y_noise[2], sd = 100)
#
#     out <- run_oxygen_forecast(bc = boundary_noise, params = wq_parameters,
#                                ini = y_noise, times = times, ice = ice_on)
#
#     result <- data.frame('Time' = time_seq,
#                          'WT_sim' = out[,2],
#                          'DO_sim' = out[,3],
#                          'id' = as.character(ii),
#                          'ini' = as.character(jj))
#     head(result)
#     result_filter = result
#     result_filter$WT_sim = kalman_filtering(time = result_filter$Time, series = result_filter$WT_sim)
#     result_filter$DO_sim = kalman_filtering(time = result_filter$Time, series = result_filter$DO_sim)
#
#     result_filter$id = ii
#
#     result_filter <- reshape2::melt(result_filter, id = c('Time', 'id', 'ini'))
#
#     if (ii == 1){
#       df = result_filter
#     } else {
#       df <- rbind(df, result_filter)
#     }
#   }
# }
#
# head(df)
# ggplot(subset(df, variable == 'WT_sim')) +
#   geom_line(aes(Time, value, col = as.factor(id))) +
#   #linetype = ini)) +
#   # geom_ribbon(aes(x = Time, ymin = min(value),
#   #                 ymax = max(value)), fill = 'grey70') +
#   labs(x = 'Simulated Time', y = 'WT in deg C')  +
#   theme_bw()+
#   guides(col=guide_legend(title="Run")) +
#   theme(legend.position="bottom")
