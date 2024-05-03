#' Estimate the yearly cycle for one station.
#' A yearly cycle is the expected temperature on each day of the year.
#'
#' The yearly cycle is taken from weather data for a+
#' given weather station by its station ID with optional start and end date filters.
#'
#' @param station_id station ID also known as WBANNO
#' @param drop_leapdays a logical evaluating to TRUE or FALSE indicating whether leapdays should be dropped or considered, respectively, in the returned dataframe.
#' @return a data frame with the following columns:
#' \describe{
#'   \item {dayofY}{The day of the year (1-365 or 366 if drop_leapdays is TRUE)}
#'   \item {avgTemp}{The expected average temperature on the day in C)}
#' }
#' @examples
#' # get yearly cycle for Ithaca, NY
#' ithaca_predictions <- get_yearly_cycle(64758)
#' print(head(ithaca_predictions))
#' ithaca_predictions_Leapyears <- get_yearly_cycle(64758,F)
#' print(head(ithaca_predictions_Leapyears))
#' @export
get_yearly_cycle <- function(station_id, drop_leapdays = T) {
  station_weather <- get_station_weather(station_id)
  if(drop_leapdays){
    station_weather$dayofY <- as.numeric(format(station_weather$LST_DATE,"%m%d"))
    station_weather <- station_weather[station_weather$dayofY !=229,]
    station_weather$dayofY <- station_weather$dayofY+20010000 #force a non leap year
    station_weather$dayofY <- as.numeric(format(strptime(station_weather$dayofY,"%Y%m%d"),"%j"))
    yearlength=365
  }
  else{
    station_weather$dayofY <- as.numeric(format(station_weather$LST_DATE,"%j"))
    yearlength=366
  }
  station_weather <- station_weather[,c("T_DAILY_AVG","dayofY")]
  #4*pi will allow for the skew between minimum and maximum
  returned_lm <- lm(T_DAILY_AVG~
                      I(sin(2*pi*dayofY/yearlength))+I(cos(2*pi*dayofY/yearlength))+
                      I(sin(4*pi*dayofY/yearlength))+I(cos(4*pi*dayofY/yearlength)),
                    data=station_weather) 
  daysofYear <- data.frame(dayofY = seq_len(yearlength))
  predictions <- returned_lm |> predict(daysofYear)
  return (data.frame(avgTemp = predictions,dayofY = names(predictions)))
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
temperature_trend <- function(station_id) {
  station_weather <- get_station_weather(station_id)
  # aggregate the temperature data to once per year
  intervals <-
    seq(min(station_weather$LST_DATE),
        max(station_weather$LST_DATE),
        by = "12 months")
  trend_for_lm <- rep(0, length(intervals) - 1)
  names(trend_for_lm) <- 1:(length(intervals) - 1)
  for (period in 1:(length(intervals) - 1)) {
    cond <-
      (station_weather$LST_DATE >= intervals[period]) &
      (station_weather$LST_DATE <= intervals[period + 1])
    trend_for_lm[period] <-
      mean(station_weather[cond, "T_DAILY_AVG"], na.rm = T)
  }
  trend_dataframe <-
    data.frame(temperature = trend_for_lm,
               annual_period = 1:(length(intervals) - 1))
  return(lm(temperature ~ annual_period, data = trend_dataframe))
}
