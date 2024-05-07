# create empty data frame to append to
daily_weather = data.frame()

# iterate through year folders
for (year in dir("raw_data/")){

  # create empty data frame for year
  year_daily_weather = data.frame()

  # iterate through station data for each year
  for (file_name in dir(paste0("raw_data/", year))){

    # get file name without an extension
    file_name_no_ext = substr(file_name, start = 1, stop = nchar(file_name) - 4)

    # parse the station name
    station_name = strsplit(file_name_no_ext, "-")[[1]][3]

    # parse the state name
    state = strsplit(station_name, "_")[[1]][1]

    # read data and set up state and station name columns
    temp_dat = read.table(paste0("raw_data/", year, "/", file_name))
    temp_dat$state = state
    temp_dat$station_name = station_name

    # append rows for this data file to the current year's weather data and reorder columns
    year_daily_weather = rbind(year_daily_weather, temp_dat[ , c(1, 29, 30, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
  }

  # append year's data to the overall daily weather data frame
  daily_weather = rbind(daily_weather, year_daily_weather)
}

# converting column names to the ones listed in the project requirements
colnames(daily_weather) = c("WBANNO", "state", "station name", "LST_DATE", "CRX_VN", "LONGITUDE", "LATITUDE",
"T_DAILY_MAX", "T_DAILY_MIN", "T_DAILY_MEAN", "T_DAILY_AVG", "P_DAILY_CALC",
"SOLARAD_DAILY")

# converting LST_DATE to R Date format
daily_weather$LST_DATE = as.Date(as.character(daily_weather$LST_DATE), "%Y%m%d")

# converting missing values to NA according to missing values listed in the data documentation
daily_weather[daily_weather == -9999] = NA
daily_weather[daily_weather$CRX_VN == -9, "CRX_VN"] = NA
daily_weather[daily_weather$LATITUDE == -99, "LATITUDE"] = NA
daily_weather[daily_weather$LONGITUDE == -99, "LONGITUDE"] = NA

# not sure what a station identifier is, assuming it is WBANNO
station_info = unique(daily_weather[ , c("WBANNO", "station name", "state", "LONGITUDE", "LATITUDE")])
row.names(station_info) = NULL

save(daily_weather, file = "ProjectSTSCI4520/data/daily_weather.RData")
save(station_info, file = "ProjectSTSCI4520/data/station_info.RData")
