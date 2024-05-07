#' Estimate the yearly cycle for one station.
#' A yearly cycle is the expected temperature on each day of the year.
#'
#' The yearly cycle is taken from weather data for a
#' given weather station by its station ID
#'
#' @param station_id numeric station ID also known as WBANNO
#' @param drop_leapdays a logical evaluating to TRUE or FALSE indicating whether leapdays should be dropped or considered, respectively, in the returned dataframe.
#' @return a data frame with the following columns:
#' \describe{
#'   \item{day_of_year}{The day of the year (1-365 or 366 if drop_leapdays is TRUE)}
#'   \item{avg_temp}{The expected average temperature on the day in C)}
#' }
#' @examples
#' # get yearly cycle for Ithaca, NY
#' ithaca_predictions <- get_yearly_cycle(64758)
#' print(head(ithaca_predictions))
#' ithaca_predictions_Leapyears <- get_yearly_cycle(64758,F)
#' print(head(ithaca_predictions_Leapyears))
#' @export
get_yearly_cycle <- function(station_id, drop_leapdays = T) {
  # check if station_id is a valid length 1 numeric
  if (length(station_id) != 1 || !is.numeric(station_id)) {
    stop("Invalid station ID: station ID must be numeric of length 1")
  }
  # check if station_id is in the dataset
  if (!station_id %in% station_info$WBANNO) {
    stop("Invalid station ID: station ID provided could not be found in the data")
  }
  # check if drop_leapdays is a valid length 1 logical
  if (length(drop_leapdays) != 1 || !is.logical(drop_leapdays)) {
    stop("Invalid drop_leapdays: drop_leapdays must be TRUE or FALSE")
  }

  # get station data
  station_weather <- get_station_weather(station_id)
  # if drop_leapdays, remove Feb 29 and add dayofY column
  if (drop_leapdays) {
    station_weather$dayofY <- as.numeric(format(station_weather$LST_DATE, "%m%d"))
    station_weather <- station_weather[station_weather$dayofY != 229, ]
    station_weather$dayofY <- station_weather$dayofY + 20010000 #force a non leap year
    station_weather$dayofY <- as.numeric(format(strptime(station_weather$dayofY, "%Y%m%d"), "%j"))
    yearlength = 365
  }
  # otherwise just add dayofY column
  else{
    station_weather$dayofY <- as.numeric(format(station_weather$LST_DATE, "%j"))
    yearlength = 366
  }
  station_weather <- station_weather[, c("T_DAILY_AVG", "dayofY")]
  #4*pi will allow for the skew between minimum and maximum
  # create a linear model predicting the daily average temperature based on day of year
  station_lm <- lm(T_DAILY_AVG ~
                     I(sin(2 * pi * dayofY / yearlength)) + I(cos(2 * pi *
                                                                    dayofY / yearlength)) +
                     I(sin(4 * pi * dayofY / yearlength)) + I(cos(4 * pi *
                                                                    dayofY / yearlength)), data = station_weather)
  # create cleaned data frame to return
  daysofYear <- data.frame(dayofY = seq_len(yearlength))
  predictions <- station_lm |> predict(daysofYear)
  return (data.frame(day_of_year = as.numeric(names(predictions)), avg_temp = predictions))
}

#' Estimate the trend over time for annual temperatures.
#' This is done through fitting a model accounting for time elapsed in years and seasonality.
#'
#'
#' @param station_id numeric station ID also known as WBANNO
#' @return A summary.lm object for the fitted model predicting the daily average from year.
#' @examples
#' # get yearly trend for Ithaca, NY
#' ithaca_model <- temperature_trend(64758)
#' print(ithaca_model)
#' @export
temperature_trend <- function(station_id) {
  # check if station_id is a valid length 1 numeric
  if (length(station_id) != 1 || !is.numeric(station_id)) {
    stop("Invalid station ID: station ID must be numeric of length 1")
  }
  # check if station_id is in the dataset
  if (!station_id %in% station_info$WBANNO) {
    stop("Invalid station ID: station ID provided could not be found in the data")
  }
  # get station data
  station_weather <- get_station_weather(station_id)
  # create years elapsed column
  station_weather$years_elapsed <- as.numeric(station_weather$LST_DATE -
                                                as.Date("2000-01-01")) / 365.25
  station_weather <- station_weather[, c("T_DAILY_AVG", "years_elapsed")]
  # create linear model predicting the average daily temperature based on the years_elapsed
  cycle_lm <- lm(T_DAILY_AVG ~
                   I(sin(2 * pi * years_elapsed)) + I(cos(2 * pi * years_elapsed)) +
                   I(sin(4 * pi * years_elapsed)) + I(cos(4 * pi * years_elapsed)) +
                   years_elapsed,
                 data = station_weather)
  # get and return the summary for our model
  return(summary(cycle_lm))
}
