#' WMO TEST
#' PRE-CHECK & CHECK1 - Gross Error Limit (GEL): MSL PRESSURE
#'
#' Input:
#' The function parameters are two data frames:
#' 'Metadata.rds' with the metadata
#' 'Digitisation.rds' with the digitised data
#' 
#' Output:
#' A file named 'Missing-pressure_StationName_YYYY.txt' with the missing
#' observations
#' A file named 'Error-pressure_StationName_YYYY.txt' with the errors
#' A file named 'ECMWF-pressure_StationName_YYYY.txt' with the observations
#' quality controled, written in ECMWF format
#' 
test_msl_pressure <- function() {
  meta <- readRDS("Metadata.rds")
  digt <- readRDS("Digitisation.rds")
  # Creates vectors with the metadata
  st_name <- meta$st_name
  year <- meta$year
  uid <- meta$uid
  lon <- meta$lon
  lat <- meta$lat
  alt <- meta$alt
  # Creates vectors with the data
  day_year <- digt$day_year
  month <- digt$month
  day <- digt$day
  hour <- digt$hour
  press <- digt$press
  # Observations 4 times a day: code 0
  time_p <- as.integer(0)
  # MSL Pressure: code 6
  var_mslp <- as.integer(6)
  # Sets flag for pressure gross error limits to default: code -999
  flg_gel_p <- as.integer(-999)
  # Flag codes:
  # 0 – Correct
  # 1 – Erroneous
  # 9 – Suspect
  # -999 - Default value / missing check
  #
  # Output data frames
  miss_obs <- data.frame()
  err_press <- data.frame()
  ecmwf_form <- data.frame()
  #
  # WMO TEST
  # PRE-CHECK & CHECK1 - Gross Error Limit (GEL): MSL PRESSURE
  for (j in 1:nrow(digt)) {
    # Latitudes belonging to the interval [-45, +45]
    if (lat >= -45 && lat <= 45) {
      # Austral Winter
      if (month[j] == 4 || month[j] == 5  || month[j] == 6 
        || month[j] == 7 || month[j] == 8 || month[j] == 9) {
        # P value is missing
        if (press[j] == -999) {
          miss_obsj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "MSL_P")
          miss_obs <- rbind(miss_obs, miss_obsj)
          # Suspect values if: 870 >= P < 910 hPa or 1080 < P <= 1100 hPa
        } else if ((press[j] >= 870 && press[j] < 910) 
          ||(press[j] > 1080 && press[j] <= 1100)) {
          flg_gel_p <- 9
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Outliers/erroneous if: P < 870 hPa or P > 1100 hPa
        } else if (press[j] < 870 || press[j] > 1100) {  
          flg_gel_p <- 1
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Correct if: 910 <= P <= 1080 hPa
        } else {
          flg_gel_p <- 0
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
        }
        # Austral Summer
      } else if (month[j] == 1 || month[j] == 2 ||month[j] == 3 
        ||month[j] == 10 ||month[j] == 11 ||month[j] == 12) {
        # P value is missing
        if (press[j] == -999) {
          miss_obsj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "MSL_P")
          miss_obs <- rbind(miss_obs, miss_obsj)
          # Suspect values if: 850 >= P < 900 hPa or 1080 < P <= 1100 hPa
        } else if ((press[j] >= 850 && press[j] < 900) 
          ||(press[j] > 1080 && press[j] <= 1100)) {
          flg_gel_p <- 9
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Outliers/erroneous if: P < 850 hPa or P > 1100 hPa
        } else if (press[j] < 850 || press[j] > 1100) {  
          flg_gel_p <- 1
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Correct if: 900 <= P <= 1080 hPa
        } else {
          flg_gel_p <- 0
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
        }
      }
      # Latitudes belonging to the interval [-90, -45[ U ]+45, +90]
    } else if ((lat >= -90 && lat < -45) || (lat > 45 && lat <= 90)) {
      # Austral Winter
      if (month[j] == 4 || month[j] == 5  || month[j] == 6 
        || month[j] == 7 || month[j] == 8 || month[j] == 9) {
        # P value is missing
        if (press[j] == -999) {
          miss_obsj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "MSL_P")
          miss_obs <- rbind(miss_obs, miss_obsj)
          # Suspect values if: 910 >= P < 940 hPa or 1080 < P <= 1100 hPa
        } else if ((press[j] >= 910 && press[j] < 940) 
          ||(press[j] > 1080 && press[j] <= 1100)) {
          flg_gel_p <- 9
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Outliers/erroneous if: P < 910 hPa or P > 1100 hPa
        } else if (press[j] < 910 || press[j] > 1100) {  
          flg_gel_p <- 1
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Correct if: 910 <= P <= 1080 hPa
        } else {
          flg_gel_p <- 0
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
        }
        # Austral Summer
      } else if (month[j] == 1 || month[j] == 2 ||month[j] == 3 
        ||month[j] == 10 ||month[j] == 11 ||month[j] == 12) {
        # P value is missing
        if (press[j] == -999) {
          miss_obsj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "MSL_P")
          miss_obs <- rbind(miss_obs, miss_obsj)
          # Suspect values if: 920 >= P < 950 hPa or 1080 < P <= 1100 hPa
        } else if ((press[j] >= 920 && press[j] < 950) 
          ||(press[j] > 1080 && press[j] <= 1100)) {
          flg_gel_p <- 9
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Outliers/erroneous if: P < 920 hPa or P > 1100 hPa
        } else if (press[j] < 920 || press[j] > 1100) {  
          flg_gel_p <- 1
          err_pressj <- data.frame(year, month[j], day[j], hour[j], 
            press[j], "GEL_MSL_P")
          err_press <- rbind(err_press, err_pressj)
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
          # Correct if: 950 <= P <= 1080 hPa
        } else {
          flg_gel_p <- 0
          ecmwf_formj <- data.frame(uid, lon, lat, alt, year, month[j], 
            day[j], hour[j], time_p, var_mslp, press[j], flg_gel_p)
          ecmwf_form <- rbind(ecmwf_form, ecmwf_formj)
        }
      }
    }
  }
  # Creates the ouput files name
  f_miss <- paste("Missing-pressure", st_name, as.character(year), sep =  "_")
  f_err <- paste("Error-pressure", st_name, as.character(year), sep =  "_")
  f_ecmwf <- paste("ECMWF-pressure", st_name, as.character(year), sep =  "_")
  # Writes the .txt files
  write.table(miss_obs, file = paste(f_miss, ".txt", sep = ""), 
    row.names = FALSE, col.names = FALSE,
    sep = "\t", quote = FALSE)
  write.table(err_press, file = paste(f_err, ".txt", sep = ""), 
    row.names = FALSE, col.names = FALSE,
    sep = "\t", quote = FALSE)
  write.table(ecmwf_form, file = paste(f_ecmwf, ".txt", sep = ""), 
    row.names = FALSE, col.names = FALSE,
    sep = "\t", quote = FALSE)
  report <- cbind(c("Pressure values processed: ",
    "Pressure values quality controled: ",
    "Pressure values suspect/erroneous: ",
    "Pressure values missing in the record: "),
    c(length(day_year), nrow(ecmwf_form), nrow(err_press), nrow(miss_obs)))
  cat("Check the files 'Missing-pressure', 'Error-pressure' and 'ECMWF-pressure'.\n\n")
  return(report)
}
