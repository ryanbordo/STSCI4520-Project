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
  return(data.frame(returned))
}


#' Plot points generated over a map of the contiguous US.
#'
#' @param interpolated_data the datapoints that are to be plotted
#' @return A plot with points overlaid.
#' @examples
#' # get a plot of the USA with interpolated temperatures
#' point_map <- interpolate_data(toInterpolate$T_DAILY_AVG,toInterpolate$LONGITUDE,toInterpolate$LATITUDE,
#'                  create_grid(resolution_X = 20,resolution_Y=20))
#' plot_interpolations(point_map)
#' @export
#'
plot_interpolations <- function(interpolated_data){
  usamap <- sf::st_transform(sf::st_as_sf(maps::map('usa',plot=F,fill=T)),crs="+proj=longlat +datum=WGS84")
  plot(sf::st_geometry(usamap),reset=T)
  plot(sf::st_as_sf(data.frame(interpolated_data),coords=c("longitudes","latitudes"),crs="+proj=longlat +datum=WGS84"),add=T,reset=F)
}
