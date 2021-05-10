
library(dplyr)
library(scales)

source('Forecasting_test1/EnKF_functions_temp_2ndtry.R')
source('Forecasting_test1/make_inflow_temp.R')

n_en = 100 # how many ensembles

n_en = n_en
start = "2016-09-14" # 2014-06-04 is earliest date
stop = "2021-02-28" # 2014-09-20 is latest date
obs_file = 'data/rds_noNA.rds'
driver_file = 'data/rds_noNA.rds'
obs_cv = 0.05 # we should make this a standard deviation if we don't think it'll change with magnitude
param_cv = 0.2
driver_cv = c(0.1) # CV of driver data for air temperature
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


# plotting

transparent = 0.2 #
mean_temp_est = apply(est_out$Y[1,,], 1, FUN = mean)
plot(mean_temp_est, type = 'l')


plot(mean_temp_est ~ est_out$dates, type ='l',
     ylim = range(c(est_out$Y[1,,], est_out$obs[1,,]), na.rm = T),
     col = 'grey', ylab = '°C', xlab = '')
for(i in 2:n_en){
  lines(est_out$Y[1,,i] ~ est_out$dates,
        col = 'grey')
}
lines(mean_temp_est ~ est_out$dates, col = 'black', lwd =1 )
points(est_out$obs[1,,] ~ est_out$dates, pch = 16, col = alpha('red', transparent))
arrows(est_out$dates, est_out$obs[1,,] - est_out$state_sd ,
       est_out$dates, est_out$obs[1,,] + est_out$state_sd,
       code = 3, length = 0.1, angle = 90, col = alpha('red', transparent))

