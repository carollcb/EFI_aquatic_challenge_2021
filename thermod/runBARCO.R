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
bound <-read_delim(paste0(folder,'/meteo.txt'), delim = '\t')

colnames(bound) <- c('Day','Jsw','Tair','Dew','vW')

# function to calculate wind shear stress (and transforming wind speed from km/h to m/s)
bound$Uw <- 19.0 + 0.95 * (bound$vW * 1000/3600)^2
bound$vW <- bound$vW * 1000/3600

boundary <- bound

# simulation maximum length
times <- seq(from = 1, to = max(boundary$Day), by = 1)

start_date <- get_yaml_value(config_file, "time", "start")
stop_date <- get_yaml_value(config_file, "time", "stop")
time_seq =seq(as.Date(start_date), as.Date(stop_date), by = 'day')

if (file.exists('output.txt')){
  file.remove('output.txt')
}

wq_parameters <- append(parameters, c(0.001 / 1000,
                                      100, 15000 * 1e4, 100))
parameters[19] = 3.2 # calibration parameter
wq_parameters[19] = parameters[19] # calibration parameter

# initial water temperatures & do
yini <- c(17, 10 * 1000/1e6  * wq_parameters[1])

ice_on = TRUE # ice "simulation" on or off?
out <- run_oxygen_forecast(bc = boundary, params = wq_parameters, ini = yini, times = times, ice = ice_on)

result <- data.frame('Time' = time_seq,
                     'WT_sim' = out[,2],
                     'DO_sim' = out[,3])
head(result)
result_filter = result
result_filter$WT_sim = kalman_filtering(time = result_filter$Time, series = result_filter$WT_sim)
result_filter$DO_sim = kalman_filtering(time = result_filter$Time, series = result_filter$DO_sim)

obs <-read_delim(paste0('obs.txt'), delim = ',')
obs.df <- data.frame('Time' = as.Date(obs$Date),
                     'WT_obs' = obs$Water_Temperature_celsius,
                     'DO_obs' = obs$Dissolved_Odxygen_milliGramPerLiter)

df <- merge(result_filter, obs.df, by = 'Time', all = TRUE)
head(df)

g1 <- ggplot(df) +
  geom_line(aes(x=Time, y=WT_sim, col='Simulated')) +
  geom_point(aes(x=Time, y=WT_obs, col='Observed')) +
  labs(x = 'Simulated Time', y = 'WT in deg C')  +
  theme_bw()+
  # xlim(as.Date('2020-02-01'), as.Date('2020-06-01')) +
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="bottom");g1
ggsave(file='BARCO_thermod_forecast_wtemp.png', g1, dpi = 300,width = 200,height = 250, units = 'mm')

g2 <- ggplot(df) +
  geom_line(aes(x=Time, y=DO_sim/ 1000 /  wq_parameters[1] * 1e6, col='Simulated')) +
  geom_point(aes(x=Time, y=DO_obs, col='Observed')) +
  labs(x = 'Simulated Time', y = 'DO in g/m3')  +
  theme_bw()+
  # xlim(as.Date('2020-02-01'), as.Date('2020-06-01')) +
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="bottom");g2
ggsave(file='BARCO_thermod_forecast_do.png', g2, dpi = 300,width = 200,height = 250, units = 'mm')

output <- read.table('output.txt')
#write.table(matrix(c(sw, lw, water_lw, conv, evap, Rh, t, ice_param,ATM, NEP), nrow=1),

output <- data.frame('sw'=output[,1],'lw'=output[,2],'water_lw'=output[,3],'conv'=output[,4],
                     'evap'=output[,5], 'Rh' = output[,6],'time' = output[,7],
                     'ice' = output[8], 'atmosp_exchange' = output[,9],
                     'NEP' = output[,10])

output$balance <- apply(output[,c(1:6)],1, sum)

g3 <- ggplot(output) +
  geom_line(aes(x = time,y = sw, col = 'Shortwave')) +
  geom_line(aes(x = time,y = lw, col = 'Longwave')) +
  geom_line(aes(x = time,y = water_lw, col = 'Reflection')) +
  geom_line(aes(x = time,y = conv, col = 'Conduction')) +
  geom_line(aes(x = time,y = evap, col = 'Evaporation')) +
  geom_line(aes(x = time,y = balance, col = 'Sum'), linetype = "dashed") +
  scale_colour_brewer("Energy terms", palette="Set3") +
  labs(x = 'Simulated Time', y = 'Fluxes in cal/(cm2 d)')  +
  theme_bw()+
  theme(legend.position="bottom");g3

g4 <- ggplot(output) +
  geom_line(aes(x = time,y = atmosp_exchange, col = 'Atmosphere')) +
  geom_line(aes(x = time,y = NEP, col = 'Productivity')) +
  scale_colour_brewer("Energy terms", palette="Set3") +
  labs(x = 'Simulated Time', y = 'Fluxes in mg/(cm3 d)')  +
  theme_bw()+
  theme(legend.position="bottom");g4
