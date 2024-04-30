#' Estimate the yearly cycle for one station.
#' A yearly cycle is the expected temperature on each day of the year.
#'
#' The yearly cycle is taken from weather data for a+
#' given weather station by its station ID with optional start and end date filters.
#'
#' @param station_id station ID also known as WBANNO
#' @param start_date optional character (YYYY-MM-DD) or date parameter to filter for days beginning with start_date, inclusive
#' @param end_date optional character (YYYY-MM-DD) or date parameter to filter for days ending before start_date, inclusive
#' @param drop_leapdays a logical evaluating to TRUE or FALSE indicating whether leapdays should be considered or dropped in the returned dataframe.
#' @return a data frame with the following columns:
#' \describe{
#'   \item {day_number}{The day of the year (1-365 or 366 if drop_leapdays is TRUE)}
#'   \item {expected_avg_temp}{The expected average temperature on the day in C)}
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
get_yearly_cycle <- function(station_id, drop_leapdays = T) {
  station_weather <- get_station_weather(station_id)
  station_weather$day_number <-
    as.numeric(strftime(station_weather$LST_DATE, format = "%j"))
  yearly_cycle_data <-
    station_weather |> dplyr:::group_by(day_number) |> dplyr:::summarize(expected_avg_temp = mean(T_DAILY_AVG, na.rm = T))
  if (drop_leapdays && nrow(station_weather) == 366) {
    yearly_cycle_data <- yearly_cycle_data[-60, ]
    yearly_cycle_data$day_number = 1:365
  }
  return (yearly_cycle_data)
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
