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



#' Plot a grid of the US with maximum resolution specified.
#' Coordinate grid points are in Mercator projection.
#'
#' @param resolution_X an integer of the maximum resolution with respect to longitude to be plotted.
#' @param resolution_Y an integer of the maximum resolution with respect to latitude to be plotted.
#' @return a set of points at the specified resolution, all falling within the contiguous USA.
#' @examples
#' # get a plot of the USA at 30 maximum points with respect to longitude and 20 with respect to latitude
#' point_map <- create_grid(resolution_X = 30, resolution_Y=20)
#' plot(point_map)
#' @export
create_grid <- function(resolution_X = 50,resolution_Y = 50){
  usamap <- sf::st_transform(sf::st_as_sf(maps::map('usa',plot=F,fill=T)),crs=3857)
  boundaries <- sf::st_bbox(usamap)
  longitudes <- seq(boundaries$xmin,boundaries$xmax,length.out=resolution_X)
  latitudes <- seq(boundaries$ymin,boundaries$ymax,length.out=resolution_Y)
  usa.grid <- expand.grid(longitudes,latitudes)
  colnames(usa.grid) <- c("longitude","latitude")
  grid_sf <- sf::st_as_sf(usa.grid,coords=c("longitude","latitude"),crs = 3857)
  filtered_points <- sf::st_filter(grid_sf,usamap)
  return(filtered_points)
}


#' Interpolate values of X and Y via a gaussian processes model given a grid to interpolate to.
#'
#' @param toInterpolate a numeric of the datapoints which are to be interpolated.
#' @param longitudes a numeric of the longitudes associated with the points to interpolate.
#' @param latitudes a numeric of the latitudes associated with the points to interpolate.
#' @param gridpoints a series of sf points associated to the grid points to be interpolated with.
#' @return a dataframe containing interpolated data, their longitudes, and their latitudes.
#' @examples
#' Interpolates a plot to the daily average temperature across the US
#' toInterpolate <- na.omit(daily_weather)
#' toInterpolate <- toInterpolate[!duplicated(toInterpolate$WBANNO),
#'                                c("LONGITUDE","LATITUDE","T_DAILY_AVG")]
#' interpolate_data(toInterpolate$T_DAILY_AVG,toInterpolate$LONGITUDE,toInterpolate$LATITUDE,
#'                  create_grid(resolution_X = 20,resolution_Y=20))
#' @export


interpolate_data <- function(toInterpolate,longitudes,latitudes,gridpoints){
  #station data should have one column of data, then the station's longitude and latitude
  #Interpolation done via gpgp
  #train the gpgp model
  gridpoints <- sf::st_transform(filtered_points,crs="+proj=longlat +datum=WGS84")
  #convert from UTM back to longitudes and latitudes
  coord <- cbind(longitudes,latitudes)
  X <- cbind(rep(1,length(toInterpolate)),coord)
  gp_model <- GpGp::fit_model(y=toInterpolate,locs=coord,X=X,
                              covfun_name = "exponential_sphere",silent=T)
  grid_matrix <-sf::st_coordinates(gridpoints)
  Xpred <- cbind(1,grid_matrix)
  interpolations <- GpGp::predictions(fit=gp_model,locs_pred= grid_matrix,
                                      X_pred=Xpred)
  returned <- cbind(interpolations,grid_matrix)
  colnames(returned) <- c("interpolations", "longitudes",'latitudes')
  return(returned)
}


#' Plot points generated over a map of the contiguous US.
#'
#' @param interpolated_data the datapoints that are to be plotted
#' @return a plot of the contiguous USA with points overlaid.
#' @examples
#' # get a plot of the USA at 30 maximum points with respect to longitude and 20 with respect to latitude
#' point_map <- create_grid(resolution_X = 30, resolution_Y=20)
#' plot_interpolations(point_map)
#' @export



plot_interpolations <- function(interpolated_data){
  usamap <- sf::st_transform(sf::st_as_sf(maps::map('usa',plot=F,fill=T)),crs=3857)
  plot(usamap,reset=F,key.pos=NULL)
  plot(interpolated_data,add=T)
}
