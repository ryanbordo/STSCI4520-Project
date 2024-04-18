dat = data.frame()

for (year in dir("raw_data")){
  for (file_name in dir(paste0("raw_data/", year))){
    file_name_no_ext = substr(file_name, start = 1, stop = nchar(file_name) - 4)
    station_name = strsplit(file_name_no_ext, "-")[[1]][3]
    state = strsplit(station_name, "_")[[1]][1]
    temp_dat = read.table(paste0("raw_data/", year, "/", file_name))
    temp_dat$state = state
    temp_dat$station_name = station_name
    dat = rbind(dat, temp_dat[ , c(1, 29, 30, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
  }
}

# converting column names to the ones listed in the project requirements
colnames(dat) = c("WBANNO", "state", "station name", "LST_DATE", "CRX_VN", "LONGITUDE", "LATITUDE",
"T_DAILY_MAX", "T_DAILY_MIN", "T_DAILY_MEAN", "T_DAILY_AVG", "P_DAILY_CALC",
"SOLARAD_DAILY")

# converting LST_DATE to R Date format
dat$LST_DATE = as.Date(as.character(dat$LST_DATE), "%Y%m%d")

# converting missing values to NA according to missing values listed in the data documentation
dat[dat == -9999] = NA
dat[dat$CRX_VN == -9, "CRX_VN"] = NA
dat[dat$LATITUDE == -99, "LATITUDE"] = NA
dat[dat$LONGITUDE == -99, "LONGITUDE"] = NA
