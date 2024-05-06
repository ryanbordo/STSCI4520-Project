test_that("temperature trend data looks ok", {
  ithaca_weather <- get_station_weather(64758)
  grid <- create_grid()
  ithaca_interp <-
    interpolate_data(
      datapoints = ithaca_weather$T_DAILY_AVG,
      latitudes = ithaca_weather$LATITUDE,
      longitudes = ithaca_weather$LONGITUDE,
      gridpoints =  grid
    )
  expect_equal(all.colnames(ithaca_interp) == c("interpolations", "longitudes", "latitudes", "inUSA"),
               TRUE)
  expect_equal(all(class(ithaca_interp$interpolations) == "numeric"),
               TRUE)
  expect_equal(nrow(ithaca_interp),
               2500)
})
