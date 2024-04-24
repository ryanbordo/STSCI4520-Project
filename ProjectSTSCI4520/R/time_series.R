#' Retrieve daily weather data from a specific station
#'
#' Get the National Centers for Environmental Information daily temperature and weather data for a
#' given weather station by its station ID with optional start and end date filters.
#'
#' @param station_id station ID also known as WBANNO
#' @param start_date optional character (YYYY-MM-DD) or date parameter to filter for days beginning with start_date, inclusive
#' @param end_date optional character (YYYY-MM-DD) or date parameter to filter for days ending before start_date, inclusive
#' @return a data frame with the following columns:
#' \describe{
#'   \item {WBANNO}{"The station WBAN number"}
#'   \item {state}{the state location of the weather station}
#'   \item {station_name} {the name of the station given by the state location vector}
#'   \item {LST_DATE} {the local standard time date of the weather entry}
#'   \item {CRX_VN} {the version number of the datalogger program at the entry time}
#'   \item {LONGITUDE} {the longitude of the station using WGS-84}
#'   \item {LATITUDE} {the latitude of the station using WGS-84}
#'   \item {T_DAILY_MAX} {the maximum air temperature observed in C}
#'   \item {T_DAILY_MIN} {the minimum air temperature observed in C}
#'   \item {T_DAILY_MEAN} {the mean air temperature observed in C given by (T_DAILY_MAX + T_DAILY_MIN) / 2}
#'   \item {T_DAILY_AVG} {the average air temperature observed in C}
#'   \item {P_DAILY_CALC} {the total amount of precipitation in mm}
#'   \item {SOLARAD_DAILY} {the total solar energy in MJ/meter^2 given by global radiation rates, conversion, and integration over time}
#' }
#' @examples
#' # get daily temperature information for Ithaca, NY
#' ithaca_data_all <- get_station_weather(64758)
#' print(head(ithaca_data_all))
#' ithaca_data_early <- get_station_weather(64758, end_date = "2010-01-01")
#' print(head(ithaca_data_early))
#' ithaca_data_late <- get_station_weather(64758, start_date = "2020-01-01")
#' print(head(ithaca_data_late))
#' ithaca_data_specific <- get_station_weather(64758, start_date = "2010-01-01", end_date = "2020-01-01")
#' print(head(ithaca_data_specific))
#' @export
get_station_weather = function(station_id, start_date = "2000-01-01", end_date = "3000-01-01"){
  cond = (daily_weather$WBANNO == station_id) & (daily_weather$LST_DATE <= end_date) & (daily_weather$LST_DATE >= start_date)
  return (daily_weather[cond, ])
}

#' Estimate the yearly cycle for one station.
#' A yearly cycle is the expected temperature on each day of the year.
#'
#' The yearly cycle is taken from weather data for a
#' given weather station by its station ID with optional start and end date filters.
#'
#' @param station_id station ID also known as WBANNO
#' @param start_date optional character (YYYY-MM-DD) or date parameter to filter for days beginning with start_date, inclusive
#' @param end_date optional character (YYYY-MM-DD) or date parameter to filter for days ending before start_date, inclusive
#' @param drop_leapdays a logical evaluating to TRUE or FALSE indicating whether leapdays should be considered or dropped in the returned dataframe.
#' @return a data frame with the following columns:
#' \describe{
#'   \item {avgTemp}{The expected temperature for a given day}
#'   \item {day}{The day of the year (1-365 or 366 if drop_leapdays is TRUE)}
#' }
#' @examples
#' # get yearly cycle for Ithaca, NY
#' ithaca_data_all <- get_yearly_cycle(64758)
#' print(head(ithaca_data_all))
#' ithaca_data_early <- get_yearly_cycle(64758, end_date = "2010-01-01")
#' print(head(ithaca_data_early))
#' ithaca_data_late <- get_yearly_cycle(64758, start_date = "2020-01-01")
#' print(head(ithaca_data_late))
#' ithaca_data_specific <- get_yearly_cycle(64758, start_date = "2010-01-01", end_date = "2020-01-01")
#' print(head(ithaca_data_specific))
#' ithaca_data_noLeap <- get_yearly_cycle(64758, start_date = "2010-01-01", end_date = "2020-01-01",drop_leapdays=T)
#' print(head(ithaca_data_noLeap))
#' @export
get_yearly_cycle <- function(station_id,start_date = "2000-01-01",end_date="3000-01-01",drop_leapdays = F){
  station_weather <- get_station_weather(station_id,start_date,end_date)
  alldays <- unique(format(station_weather$LST_DATE,"%m%d"))
  if(drop_leapdays){
    alldays <- alldays[!alldays=="0229"]
  }
  returned <- rep(0,length(alldays))
  names(returned) <- alldays
  for(date in alldays){
    returned[date] <- mean(station_weather[format(station_weather$LST_DATE,"%m%d") == date,"T_DAILY_AVG"],na.rm=T)
  }
  returneddf <- data.frame(avgTemp = returned)
  returneddf$day <- as.integer(format(as.Date(rownames(returneddf),format="%m%d"),"%j"))
  if(drop_leapdays){
    returneddf$day[returneddf$day>60] <- returneddf$day[returneddf$day>60]-1
  }
  return(returneddf[order(returneddf$day),])
}
