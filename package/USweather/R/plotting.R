#' Plot a grid of the US with maximum resolution specified.
#' Coordinate grid points are in Mercator projection.
#'
#' @param resolution_X an integer of the maximum resolution with respect to longitude to be plotted.
#' @param resolution_Y an integer of the maximum resolution with respect to latitude to be plotted.
#' @return a set of points at the specified resolution with a logical column representing whether the point falls within the contiguous USA.
#' @examples
#' # get a plot of the USA at 30 maximum points with respect to longitude and 20 with respect to latitude
#' point_map <- create_grid(resolution_X = 30, resolution_Y=20)
#' plot(point_map)
#' @export
create_grid <- function(resolution_X = 50,
                        resolution_Y = 50) {
  # make sure resolution_X is a valid length 1 integer
  if (length(resolution_X) != 1 ||
      !is.numeric(resolution_X) || resolution_X %% 1 != 0) {
    stop("Invalid resolution_X: resolution_X must be integer of length 1")
  }
  # make sure resolution_Y is a valid length 1 integer
  if (length(resolution_Y) != 1 ||
      !is.numeric(resolution_Y) || resolution_Y %% 1 != 0) {
    stop("Invalid resolution_Y: resolution_Y must be integer of length 1")
  }
  # get map of contiguous US
  usamap <-
    sf::st_transform(sf::st_as_sf(maps::map(
      'usa', regions = 'main', plot = F
    )), crs = 4326)
  # find boundaries of contiguous US
  boundaries <- sf::st_bbox(usamap)
  # find the coords of the contiguous US
  usacoords <- data.frame(sf::st_coordinates(usamap))
  # find the longitudes and latitudes of the US coordinates
  longitudes <-
    seq(boundaries$xmin, boundaries$xmax, length.out = resolution_X)
  latitudes <-
    seq(boundaries$ymin, boundaries$ymax, length.out = resolution_Y)
  # expand these to a grid
  usa.grid <- expand.grid(longitudes, latitudes)
  colnames(usa.grid) <- c("longitude", "latitude")
  grid_sf <- sf::st_as_sf(usa.grid,
                          coords = c("longitude", "latitude"),
                          crs = 4326)
  # create a column for whether the point is in the contiguous US
  grid_sf$inUSA <-
    sp::point.in.polygon(usa.grid$longitude,
                         usa.grid$latitude,
                         usacoords$X,
                         usacoords$Y)
  return(grid_sf)
}


#' Interpolate values of X and Y via a gaussian processes model given a grid to interpolate to.
#'
#' @param formula a object of class "formula" or one that can be coerced to that class, making a description of the datapoints for interpolation.
#' @param data a dataframe containing the points to be used for interpolation
#' @param long_lat_names an optional (default to c("LONGITUDE","LATITUDE")) character vector specifying the names of the longitude and latitude columns in the data respectively to be used as locations
#' @param gridpoints a series of sf points associated to the grid points to be interpolated with. Must have matching vectors as non-location covariates.
#' @return a dataframe containing the following columns:
#' \describe{
#'   \item {interpolations}: {The numeric interpolated data points to plotted}
#'   \item {longitudes}: {A numeric of the longitudes to plot}
#'   \item {latitudes}: {A numeric of the latitudes to plot}
#'   \item {inUSA}: {A binary variable whether or not to plot (within contiguous USA)}
#'   }
#' @examples
#' # Interpolates a plot to the daily average temperature across the US
#' toInterpolate <- na.omit(daily_weather)
#' toInterpolate <- toInterpolate[!duplicated(toInterpolate$WBANNO),c("LONGITUDE","LATITUDE","T_DAILY_AVG")]
#' interpolate_data(T_DAILY_AVG~LONGITUDE+LATITUDE,data=toInterpolate, gridpoints = create_grid(resolution_X=10,resolution_Y=10))
#' @export

interpolate_data <-
  function(formula, data, #formula and model.matrix
           long_lat_names = c("LONGITUDE","LATITUDE"),
           gridpoints) {
    # check if long_lat_names is a valid length 2 character vector
    if (length(long_lat_names) != 2 || !is.character(long_lat_names)) {
      stop("Invalid long_lat_names: long_lat_names must be character vector of length 2")
    }
    # check if gridpoints are a valid sf object
    if (length(gridpoints) == 0 ||
        any(class(gridpoints) != c("sf", "data.frame")) ||
        length(gridpoints$geometry) == 0) {
      stop(
        "Invalid gridpoints: gridpoints must be an sf and data.frame object with at least length 1"
      )
    }
    # omit NA data
    data <- na.omit(data)
    # get long, lat from gridpoints
    gridcoords <- sf::st_coordinates(gridpoints)
    coordinates <- cbind(LONGITUDE = data$LONGITUDE,LATITUDE = data$LATITUDE)

    # process formula to filter data
    formula <- as.formula(formula)
    covars <- strsplit(as.character(formula)[3], " \\+ ")[[1]]
    non_loc_covars <- covars[!covars %in% long_lat_names]
    fitting_data <- model.matrix(formula,data=data)

    # fit model
    gp_model <- GpGp::fit_model(y=model.extract(model.frame(formula,data=data),"response"),
                                locs=coordinates,
                                X=fitting_data,
                                covfun_name="exponential_sphere",
                                silent=T)
    # select covariate columns from gridpoints
    Xpred = cbind(1, gridcoords)
    tryCatch(
      expr = {
        Xpred <- cbind(Xpred, gridpoints[[non_loc_covars]])
      },
      error = function(cnd){
        stop("A covariate specified was not found in gridpoints")
      }
    )
    # find interpolations by using grid predictions
    interpolations <-
      GpGp::predictions(fit = gp_model,
                        locs_pred = gridcoords,
                        X_pred = Xpred)
    returned <- cbind(interpolations, gridcoords, gridpoints$inUSA)
    colnames(returned) <-
      c("interpolations", "longitudes", 'latitudes', 'inUSA')
    return(data.frame(returned))
  }

#' Plot points generated over a map of the contiguous US.
#'
#' @param interpolated_data the datapoints that are to be plotted with the following columns
#' \describe{
#'   \item {interpolations}: {The interpolated data points to plotted}
#'   \item {longitudes}: {A numeric of the longitudes to plot}
#'   \item {latitudes}: {A numeric of the latitudes to plot}
#'   \item {inUSA}: {A binary variable whether or not to plot (within contiguous USA)}
#'   }
#' @param latitudes a numeric of the latitudes associated with the points to interpolate
#' @return A plot with points overlaid.
#' @examples
#' # get a plot of the USA with interpolated temperatures from January 1st 2020
#' toInterpolate = daily_weather[daily_weather$LST_DATE == "2020-01-01", ]
#' point_map <- interpolate_data(toInterpolate$T_DAILY_AVG,toInterpolate$LONGITUDE,toInterpolate$LATITUDE,
#'                  create_grid(resolution_X = 20,resolution_Y=20))
#' plot_interpolations(point_map)
#' @export
plot_interpolations <- function(interpolated_data) {
  # check if interpolated data is valid data frame with correct columns
  if (length(interpolated_data) != 4 ||
      !is.data.frame(interpolated_data) ||
      any(
        colnames(interpolated_data) != c("interpolations", "longitudes", "latitudes", "inUSA")
      )) {
    stop(
      "Invalid interpolated_data: interpolated_data must be data frame with columns interpolations, longitudes, latitudes, and inUSA"
    )
  # check if interpolations are valid numerics
  }
  if (length(interpolated_data$interpolations) == 0 ||
      !is.numeric(interpolated_data$interpolations)) {
    stop("Invalid interpolations: interpolations must be numeric of at least length 1")
  # check if longitudes are valid numerics
  }
  if (!is.numeric(interpolated_data$longitudes)) {
    stop("Invalid longitudes: longitudes must be numeric")
  }
  # check if latitudes are valid numerics
  if (!is.numeric(interpolated_data$latitudes)) {
    stop("Invalid latitudes: latitudes must be numeric")
  }
  # check if inUSA is valid binary
  if (!all(interpolated_data$inUSA %in% c(0, 1))) {
    stop("Invalid inUSA: inUSA must be binary")
  }
  # get map of USA
  usamap <-
    sf::st_transform(sf::st_as_sf(maps::map('usa', plot = F)), crs = 4326)
  #make all invalid interpolations NA
  interpolated_data[interpolated_data$inUSA == 0, "interpolations"] <-
    NA
  longitude = unique(interpolated_data$longitudes)
  latitude = unique(interpolated_data$latitudes)
  # plot interpolations on map
  fields::imagePlot(longitude,
                    latitude,
                    matrix(interpolated_data$interpolations, nrow = length(unique(
                      interpolated_data$longitudes
                    ))))
  # add US map to plot
  plot(sf::st_geometry(usamap), add = T, )
}
