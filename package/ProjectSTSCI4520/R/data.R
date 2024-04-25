#' Daily weather data for stations in the US
#'
#' A daily dataset built from National Centers for Environmental Information daily temperature and weather data with US weather stations from 2020 to 2024
#'
#' @format a dataframe with 1134352 rows and 13 columns
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
"daily_weather"

#' Weather station information
#'
#' A dataset of National Centers for Environmental Information weather stations with name and location information recorded from 2020 to 2024
#'
#' @format a dataframe with 273 rows and 5 columns
#' \describe{
#'   \item {WBANNO}{"The station WBAN number"}
#'   \item {state}{the state location of the weather station}
#'   \item {station_name} {the name of the station given by the state location vector}
#'   \item {LST_DATE} {the local standard time date of the weather entry}
#'   \item {LONGITUDE} {the longitude of the station using WGS-84}
#'   \item {LATITUDE} {the latitude of the station using WGS-84}
#' }
"station_info"
