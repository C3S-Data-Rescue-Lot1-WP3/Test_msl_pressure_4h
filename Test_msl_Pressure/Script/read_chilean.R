#'
#' Quality Control of Surface Meteorological Records from Chilean Air Force 
#' 1950-1958
#' 
#' Reads the handwritten digitisations of Chilean records and creates two R
#' objects: one with the metadata and other with the meteorological data to be
#' quality controled
#' 
#' Input:
#'
#' One metadata file per station-year named 'StationName_Obs_YYYY.txt'.
#' The metadata file has the following constants per station-year:
#' st_name - Meteorological station name
#' year - Observations year
#' uid - Unique identifier
#' lon - Longitude
#' lat - Latitude
#' alt - Altitude
#' 
#' Twelve digitisation tables per station-year, i.e. one table for each month,
#' named 'StationName_Mth_YYYY.txt'. The digitisation tables have 13 columns
#' which correspond to the following variables:
#' day - Day of the observation
#' hour - Observation time in the format HHMM
#' dewpt- Dew point temperature in Celsius degrees (ºC)
#' cloud - Cloud cover in oktas: {0,1,2,3,4,5,6,7,8,9}
#' windir - Wind direction:
#' {N,NNE,NE,ENE,E,ESE,SE,SSE,S,SSW,SW,WSW,W,WNW,NW,NNW,C}
#' windsp - Wind speed in knots (~0,514 m/s)
#' press_msl / press_st - Atmospheric pressure (at mean sea level or station
#' level) in hectopascals (hPa)
#' temp - Air temperature in Celsius degrees (ºC)
#' prec1 - Acumulated precipitation in milimeters (mm) usually measured at
#' 12:00 UTC
#' prec2 - Acumulated precipitation in milimeters (mm) usually measured at
#' 23:00 UTC
#' min_temp - Minimum temperature in Celsius degrees (ºC) usually measured at
#' 12:00 UTC
#' max_temp - Maximum temperature in Celsius degrees (ºC) usually measured at
#' 23:00 UTC
#' rel_hum - Relative humidity in percent (%)
#'
#' Output:
#' 
#' Two data frames in .rds format:
#' One named 'Meta_StationName_YYYY.rds' with the metadata
#' Other named 'Digit_StationName_YYYY.rds' with the meterological data
#' 
read_chilean <- function() {
  # Loads the file 'StationName_Obs_Year.txt'
  dig_obs <- read.table(file.choose(), header = FALSE, sep = "\t", dec = ".", 
    quote = "", nrows = 6, stringsAsFactors = FALSE)
  # print(dig_obs)
  # Station name
  st_name <- as.character(dig_obs[1, 1])
  # Year
  year <- as.integer(dig_obs[2, 1])
  # Unique Record Identifier (UID)
  uid <- as.integer(dig_obs[3, 1])
  # Longitude in decimal degrees
  lon <- as.double(dig_obs[4, 1])
  # Latitude in decimal degrees   
  lat <- as.double(dig_obs[5, 1]) 
  # Altitude in meters
  alt <- as.double(dig_obs[6, 1])
  # Tests year
  if (year < 1950 || year > 1958) {
    cat("The year is not valid for this record.\n")
    return(year)
  }
  # Tests coordinates
  if (lon < -180 || lon > 180) {
    cat("The longitude value is out of bounds.\n")
    return(lon)
  }
  if (lat < -90 || lat > 90) {
    cat("The latitude value is out of bounds.\n")
    return(lat)
  }
  # Creates the data frame with the metadata
  meta <- data.frame(st_name, year, uid, lon, lat, alt, 
    stringsAsFactors = FALSE)
  str_meta <- str(meta)
  # Saves dataframe as .rds
  saveRDS(meta, file = "Metadata.rds")
  # Concatenates to obtain the file name
  f_name <- paste(st_name, month.abb, as.character(year), sep =  "_")
  f_namee <- paste(f_name, ".txt", sep = "")
  # Creates a data frame to receive the input data
  dig_tb13 <- data.frame()
  # Tests if is leap year
  if (year%%4 == 0) {
    # Number of days of the year
    ndays <- 1:366
    # Observations 4 times a day
    ndays4 <- rep(ndays, each = 4)
    # Number of the month (1:12) times the number of the days of the month
    nmonth <- c(rep(1, times = 31), rep(2, times = 29), rep(3, times = 31), 
      rep(4, times = 30), rep(5, times = 31), rep(6, times = 30), 
      rep(7, times = 31), rep(8, times = 31), rep(9, times = 30), 
      rep(10, times = 31), rep(11, times = 30), rep(12, times = 31))
    # Each number of the month repeated 4 times
    nmonth4 <- rep(nmonth, each = 4)
    # Creates the day to compare with the digitised value
    aday <- c(rep(1:31, each = 4), rep(1:29, each = 4), rep(1:31, each = 4), 
      rep(1:30, each = 4), rep(1:31, each = 4), rep(1:30, each = 4), 
      rep(1:31, each = 4), rep(1:31, each = 4), rep(1:30, each = 4), 
      rep(1:31, each = 4), rep(1:30, each = 4), rep(1:31, each = 4))
    # length(aday)
  } else {
    ndays <- 1:365
    ndays4 <- rep(ndays, each = 4)
    nmonth <- c(rep(1, times = 31), rep(2, times = 28), rep(3, times = 31), 
      rep(4, times = 30), rep(5, times = 31), rep(6, times = 30), 
      rep(7, times = 31), rep(8, times = 31), rep(9, times = 30), 
      rep(10, times = 31), rep(11, times = 30), rep(12, times = 31))
    nmonth4 <- rep(nmonth, each = 4)
    aday <- c(rep(1:31, each = 4), rep(1:28, each = 4), rep(1:31, each = 4), 
      rep(1:30, each = 4), rep(1:31, each = 4), rep(1:30, each = 4), 
      rep(1:31, each = 4), rep(1:31, each = 4), rep(1:30, each = 4), 
      rep(1:31, each = 4), rep(1:30, each = 4), rep(1:31, each = 4))
    # length(aday)
  }
  # Reads the 12 input .txt files and creates a data frame with all the data
  for (i in 1:12) {
    dfi <- read.table(f_namee[i], header = TRUE, sep = "\t", 
      dec = ".", quote = "",  skip = 5, stringsAsFactors = FALSE)
    dig_tb13 <- rbind(dig_tb13, dfi)
  }
  # New variables: day of the year and number of the month
  # Sets new variables type
  day_year <- as.integer(ndays4)
  month <- as.integer(nmonth4)
  # Sets columns name for digitisation table
  dig_tb13_names <- c("day", "hour", "dewpt", "cloud", "windir", 
    "windsp", "press", "temp", "prec1", "prec2", "min_temp", "max_temp", 
    "rel_hum")
  names(dig_tb13) <- dig_tb13_names
  # Sets digitised variables type
  # The coercion brings problems (NA) when there is typing errors
  day <- as.integer(dig_tb13$day)
  hour <- as.character(dig_tb13$hour) # Test in a function apart
  dewpt <- as.double(dig_tb13$dewpt) # Each of the following have standard tests
  cloud <- as.double(dig_tb13$cloud)
  windir <- as.character(dig_tb13$windir)
  windsp <- as.double(dig_tb13$windsp)
  press <- as.double(dig_tb13$press)
  temp <- as.double(dig_tb13$temp)
  prec1 <- as.double(dig_tb13$prec1)
  prec2 <- as.double(dig_tb13$prec2)
  min_temp <- as.double(dig_tb13$min_temp)
  max_temp <- as.double(dig_tb13$max_temp)
  rel_hum <- as.double(dig_tb13$rel_hum)
  # Data frame with 15th columns containing the digitisations
  dig_tb15 <- data.frame(day_year, month, day, hour, dewpt, cloud, windir, 
    windsp, press, temp, prec1, prec2, min_temp, max_temp, rel_hum,
    stringsAsFactors = FALSE)
  # Solves typing errors in the day, i.e., missing values (NA) or invalid values
  if (!identical(dig_tb15$day, aday)) {
    dig_tb15$day <- aday
  }
  # Tests for NA which correspond to typing errors
  if (anyNA(dig_tb15)) {
    hour_na <- dig_tb15[is.na(dig_tb15$hour), ]
    dewpt_na <- dig_tb15[is.na(dig_tb15$dewpt), ]
    cloud_na <- dig_tb15[is.na(dig_tb15$cloud), ]
    windir_na <- dig_tb15[is.na(dig_tb15$windir), ]
    windsp_na <- dig_tb15[is.na(dig_tb15$windsp), ]
    press_na <- dig_tb15[is.na(dig_tb15$press), ]
    temp_na <- dig_tb15[is.na(dig_tb15$temp), ]
    prec1_na <- dig_tb15[is.na(dig_tb15$prec1), ]
    prec2_na <- dig_tb15[is.na(dig_tb15$prec2), ]
    min_temp_na <- dig_tb15[is.na(dig_tb15$min_temp), ]
    max_temp_na <- dig_tb15[is.na(dig_tb15$max_temp), ]
    rel_hum_na <- dig_tb15[is.na(dig_tb15$rel_hum), ]
    # Data frame with the NA 
    dig_tb15_na <- rbind(hour_na, dewpt_na, cloud_na, windir_na, 
      windsp_na, press_na, temp_na, prec1_na, prec2_na, min_temp_na, 
      max_temp_na, rel_hum_na)
    # Replaces NA for missing values code -999
    for (i in 1:nrow(dig_tb15)) {
      # Have to test the digitised hour for errors
      # ....
      if (is.na(dig_tb15$hour[i])) {
        dig_tb15$hour[i] <- "-999"
      }
      if (is.na(dig_tb15$dewpt[i])) {
        dig_tb15$dewpt[i] <- -999
      }
      if (is.na(dig_tb15$cloud[i])) {
        dig_tb15$cloud[i] <- -999
      }
      if (is.na(dig_tb15$windir[i])) {
        dig_tb15$windir[i] <- "-999"
      }
      if (is.na(dig_tb15$windsp[i])) {
        dig_tb15$windsp[i] <- -999
      }
      if (is.na(dig_tb15$press[i])) {
        dig_tb15$press[i] <- -999
      }
      if (is.na(dig_tb15$temp[i])) {
        dig_tb15$temp[i] <- -999
      }
      if (is.na(dig_tb15$prec1[i])) {
        dig_tb15$prec1[i] <- -999
      }
      if (is.na(dig_tb15$prec2[i])) {
        dig_tb15$prec2[i] <- -999
      }
      if (is.na(dig_tb15$min_temp[i])) {
        dig_tb15$min_temp[i] <- -999
      }
      if (is.na(dig_tb15$max_temp[i])) {
        dig_tb15$max_temp[i] <- -999
      }
      if (is.na(dig_tb15$rel_hum[i])) {
        dig_tb15$rel_hum[i] <- -999
      }
    }
    cat("NA values resulting from typing errors were originated.\n")
    cat("Those values were replaced by the missing value code '-999'.\n")
    cat("Check 'Typing-errors_SationName_YYYY.txt'.\n\n")
    f_type <- paste("Typing-errors", st_name, as.character(year), sep =  "_")
    write.table(dig_tb15_na, file = paste(f_type, ".txt", sep = ""), 
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  # anyNA(dig_tb15)
  str_digt <- str(dig_tb15)
  # Saves the data frame as .rds
  saveRDS(dig_tb15, file = "Digitisation.rds")
  cat("Two data frames were created:\n")
  cat(" - 'Metadata.rds' with the metadata;\n")
  cat(" - 'Digitisation.rds' with the digitised data.\n")
  cat("Those are the input of test_msl_pressure().\n\n")
}
