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




#' Estimate the trend over time for annual temperatures.
#' This is done through annualizing the data to account for yearly cycles.
#'
#'
#' @param station_id station ID also known as WBANNO
#' @return a linear regression model trained on annualized data
#' @examples
#' # get yearly trend for Ithaca, NY
#' ithaca_model <- temperature_trend(64758)
#' print(summary(ithaca_model))
#' @export
temperature_trend <- function(station_id){
  station_weather <- get_station_weather(station_id)
  #aggregate the temperature data to once per year
  intervals <- seq(min(station_weather$LST_DATE),max(station_weather$LST_DATE),by="12 months")
  trend_for_lm <- rep(0,length(intervals)-1)
  names(trend_for_lm) <- seq_len(length(intervals)-1)
  for(period in seq_len(length(intervals)-1)){
    cond <- (station_weather$LST_DATE>=intervals[period]) & (station_weather$LST_DATE<=intervals[period+1])
    trend_for_lm[period] <- mean(station_weather[cond,"T_DAILY_AVG"],na.rm=T)
  }
  trend_dataframe <- data.frame(temperature=trend_for_lm,annual_period=seq_len(length(intervals)-1))
  return(lm(temperature~annual_period,data=trend_dataframe))
}
