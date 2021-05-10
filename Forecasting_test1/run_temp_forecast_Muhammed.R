readRDS('rds_noNA.rds')


library(dplyr)

setwd('C:/Users/User/Desktop/Forecasting/Forecasting_test1/')
source('EnKF_functions_temp_2ndtry.R')
source('make_inflow_temp.R')

n_en = 100 # how many ensembles

n_en = n_en
start = "2016-09-14" # 2014-06-04 is earliest date
stop = "2021-02-28" # 2014-09-20 is latest date
obs_file = 'rds_noNA.rds'
driver_file = 'rds_noNA.rds'
obs_cv = 0.1
param_cv = 0.2
driver_cv = c(0.2) # CV of driver data for DOC Load, Discharge out, and Lake volume, respectively
init_cond_cv = .1
n_states_est = 1
n_params_est = 0
n_params_obs = 0

est_out = EnKF(n_en = n_en,
               start = start, # 2014-06-04 is earliest date
               stop = stop, # 2014-09-20 is latest date
               obs_file = obs_file,
               driver_file = driver_file,
               obs_cv = obs_cv,
               param_cv = param_cv,
               driver_cv = driver_cv, # CV of driver data for DOC Load, Discharge out, and Lake volume, respectively
               init_cond_cv = init_cond_cv,
               n_states_est = n_states_est,
               n_params_est = n_params_est,
               n_params_obs = n_params_obs)



# EnKF = function(n_en = 100,
#                 start,
#                 stop,
#                 time_step = 'days',
#                 obs_file = 'A_EcoForecast/Data/lake_c_data.rds',
#                 driver_file = 'A_EcoForecast/Data/lake_c_data.rds',
#                 n_states_est = 1,
#                 n_params_est = 0,
#                 n_params_obs = 0,
#                 obs_cv = 0.1,
#                 param_cv = 0,
#                 driver_cv = c(0.2),
#                 init_cond_cv = 0.1){



# plotting
#mean_temp_est = apply(est_out$Y[1,,] / est_out$drivers[,1,] * 12, 1, FUN = mean)
#plot(mean_temp_est, ylim = c(-10,30))
mean_temp_est = apply(est_out$Y[1,,], 1, FUN = mean)
plot(mean_temp_est, type = 'l')

plot(mean_temp_est ~ est_out$dates, type ='l',
     ylim = range(c(est_out$Y[1,,] / est_out$drivers[,1,] * 12,
                    est_out$obs[1,,] / apply(est_out$drivers[,1,], 1, FUN = mean) * 12), na.rm = T),
     col = 'grey', ylab = '°C', xlab = '')
for(i in 2:n_en){
  lines(est_out$Y[1,,i] / est_out$drivers[,1,i] * 12 ~ est_out$dates,
        col = 'grey')
}
lines(mean_temp_est ~ est_out$dates, col = 'black', lwd =2 )
points(est_out$obs[1,,] / apply(est_out$drivers[,1,], 1, FUN = mean) * 12 ~ est_out$dates, pch = 16, col = 'red')
arrows(est_out$dates, est_out$obs[1,,] / apply(est_out$drivers[,1,], 1, FUN = mean) * 12 -
         est_out$state_sd / apply(est_out$drivers[,1,], 1, FUN = mean) * 12,
       est_out$dates, est_out$obs[1,,] / apply(est_out$drivers[,1,], 1, FUN = mean) * 12 +
         est_out$state_sd / apply(est_out$drivers[,1,], 1, FUN = mean) * 12,
       code = 3, length = 0.1, angle = 90, col = 'red')

mean_decay_est = apply(est_out$Y[1,,], 1, FUN = mean)
plot(mean_decay_est ~ est_out$dates, type ='l',
     ylim = range(est_out$Y[1,,]),
     col = 'grey', ylab = 'temp Decay (day^-1)', xlab ='')
for(i in 2:n_en){
  lines(est_out$Y[1,,i] ~ est_out$dates, col = 'grey')
}
lines(mean_decay_est ~ est_out$dates, col = 'black', lwd = 2)
