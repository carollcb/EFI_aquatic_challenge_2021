message(paste0("Running Creating Aquatics Targets at ", Sys.time()))
#remotes::install_github("cboettig/neonstore")

devtools::install_deps()

## 02_generate_targets_aquatics
## Process the raw data into the target variable product
library(neonstore)
library(tidyverse)
library(lubridate)
library(contentid)

run_full_workflow <- TRUE
generate_null <- TRUE


# Aquatic
#DP1.20053.001
#DP1.20288.001
sites <- c("BARC", "POSE")
start_date <- NA

# get NOA meteorological data
download_noaa_files_s3 <- function(siteID, date, cycle, local_directory){

  Sys.setenv("AWS_DEFAULT_REGION" = "data",
             "AWS_S3_ENDPOINT" = "ecoforecast.org")

  object <- aws.s3::get_bucket("drivers", prefix=paste0("noaa/NOAAGEFS_1hr/",siteID,"/",date,"/",cycle))

  for(i in 1:length(object)){
    aws.s3::save_object(object[[i]], bucket = "drivers", file = file.path(local_directory, object[[i]]$Key))
  }
}

noaa_gefs_read <- function(base_dir, date, cycle, sites){

  if(!(cycle %in% c("00","06","12","18"))){
    stop("cycle not available cycles of 00, 06,12,18")
  }

  cf_met_vars <- c("air_temperature",
                   "surface_downwelling_shortwave_flux_in_air",
                   "surface_downwelling_longwave_flux_in_air",
                   "relative_humidity",
                   "wind_speed",
                   "precipitation_flux")

  combined_met <- NULL

  for(i in 1:length(sites)){

    forecast_dir <- file.path(base_dir, sites[i], lubridate::as_date(date),cycle)

    forecast_files <- list.files(forecast_dir, full.names = TRUE)

    if(length(forecast_files) == 0){
      stop(paste0("no files in ", forecast_dir))
    }

    nfiles <-   length(forecast_files)

    for(j in 1:nfiles){

      ens <- dplyr::last(unlist(stringr::str_split(basename(forecast_files[j]),"_")))
      ens <- stringr::str_sub(ens,1,5)
      noaa_met_nc <- ncdf4::nc_open(forecast_files[j])
      noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
      origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
      origin <- lubridate::ymd_hm(origin)
      noaa_met_time <- origin + lubridate::hours(noaa_met_time)
      noaa_met <- tibble::tibble(time = noaa_met_time)

      for(v in 1:length(cf_met_vars)){
        noaa_met <- cbind(noaa_met, ncdf4::ncvar_get(noaa_met_nc, cf_met_vars[v]))
      }

      ncdf4::nc_close(noaa_met_nc)

      names(noaa_met) <- c("time", cf_met_vars)

      noaa_met <- noaa_met %>%
        dplyr::mutate(siteID = sites[i],
                      ensemble = as.numeric(stringr::str_sub(ens,4,5))) %>%
        dplyr::select("siteID","ensemble","time",all_of(cf_met_vars))

      combined_met <- rbind(combined_met, noaa_met)

    }
  }
  return(combined_met)
}

download_noaa_files_s3(siteID = sites[1], date = "2020-11-10", cycle = "06", local_directory <- "./drivers")

base_dir <- "./drivers/noaa/NOAAGEFS_1hr"

date <- "2020-11-10"

cycle <- "06"

meteo_input <- noaa_gefs_read(base_dir, date, cycle, sites)
head(meteo_input)

message("Downloading: DP1.20288.001")
new_data1 <- neonstore::neon_download("DP1.20288.001",site = sites, type = "basic", start_date = start_date, .token = Sys.getenv("NEON_TOKEN"))
message("Downloading: DP1.20264.001")
new_data2 <- neonstore::neon_download("DP1.20264.001", site =  sites, type = "basic", start_date = start_date, .token = Sys.getenv("NEON_TOKEN"))
message("Downloading: DP1.20053.001")
new_data3 <- neonstore::neon_download("DP1.20053.001", site =  sites, type = "basic", start_date = start_date, .token = Sys.getenv("NEON_TOKEN"))


if(!is.null(new_data1) | !is.null(new_data2) | !is.null(new_data3) | run_full_workflow){

  message(paste0("Running Creating Aquatics Targets at ", Sys.time()))

  source("02_generate_targets_aquatics.R")

  message(paste0("Completed Aquatics Target at ", Sys.time()))

  if(generate_null){

    message(paste0("Running Creating Aquatics Null at ", Sys.time()))
    source("03_generate_null_forecast_aquatics.R")
  }

}
