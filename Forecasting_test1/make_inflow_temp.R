# a function to calculate inflow temperature based on air temperature

predict_inflow_temp <- function(airtemp, inflow_temp){


  temp_predict<- 2+0.76*airtemp


  temp_predict[which(temp_predict<0)] <- 0
  return(temp_predict)
}
