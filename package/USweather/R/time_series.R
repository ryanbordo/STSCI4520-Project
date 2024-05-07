#' Retrieve daily weather data from a specific station
#'
#' Get the National Centers for Environmental Information daily temperature and weather data for a
#' given weather station by its station ID with optional start and end date filters.
#'
#' @param station_id station ID also known as WBANNO
#' @param start_date optional character (YYYY-MM-DD) or date parameter to filter for days beginning with start_date, inclusive
#' @param end_date optional character (YYYY-MM-DD) or date parameter to filter for days ending before start_date, inclusive
#' @return a data frame with the following columns:
#' \itemize{
#'   \item{WBANNO}: {The station WBAN number}
#'   \item{state}: {the state location of the weather station}
#'   \item{station_name}: {the name of the station given by the state location vector}
#'   \item{LST_DATE}: {the local standard time date of the weather entry}
#'   \item{CRX_VN}: {the version number of the datalogger program at the entry time}
#'   \item{LONGITUDE}: {the longitude of the station using WGS-84}
#'   \item{LATITUDE}: {the latitude of the station using WGS-84}
#'   \item{T_DAILY_MAX}: {the maximum air temperature observed in C}
#'   \item{T_DAILY_MIN}: {the minimum air temperature observed in C}
#'   \item{T_DAILY_MEAN}: {the mean air temperature observed in C given by (T_DAILY_MAX + T_DAILY_MIN) / 2}
#'   \item{T_DAILY_AVG}: {the average air temperature observed in C}
#'   \item{P_DAILY_CALC}: {the total amount of precipitation in mm}
#'   \item{SOLARAD_DAILY}: {the total solar energy in MJ/meter^2 given by global radiation rates, conversion, and integration over time}
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
get_station_weather <-
  function(station_id,
           start_date = "2000-01-01",
           end_date = "3000-01-01") {
    if (length(station_id) != 1 || !is.numeric(station_id)) {
      stop("Invalid station ID: station ID must be numeric of length 1")
    }
    if (!station_id %in% station_info$WBANNO) {
      stop("Invalid station ID: station ID provided could not be found in the data")
    }
    if (length(start_date) != 1 || length(end_date) != 1) {
      stop(
        "Invalid date formats: optional start_date and end_date must be single character \"YYYY-MM-DD\" or Date format"
      )
    }
    if (!class(start_date) %in% c("character", "Date") ||
        !class(end_date) %in% c("character", "Date")) {
      stop(
        "Invalid date formats: optional start_date and end_date must be character \"YYYY-MM-DD\" or Date format"
      )
    }
    if (class(start_date) == "character") {
      tryCatch(
        expr = as.Date(start_date, tryFormats = c("%Y-%m-%d")),
        error = function(cnd) {
          stop(
            "Invalid start_date format: optional start_date must be character \"YYYY-MM-DD\" or Date format"
          )
        }
      )
    }
    if (class(end_date) == "character") {
      tryCatch(
        expr = as.Date(end_date, tryFormats = c("%Y-%m-%d")),
        error = function(cnd) {
          stop(
            "Invalid end_date format: optional end_date must be character \"YYYY-MM-DD\" or Date format"
          )
        }
      )
    }

    cond <-
      (daily_weather$WBANNO == station_id) &
      (daily_weather$LST_DATE <= end_date) &
      (daily_weather$LST_DATE >= start_date)
    return (daily_weather[cond,])
  }
