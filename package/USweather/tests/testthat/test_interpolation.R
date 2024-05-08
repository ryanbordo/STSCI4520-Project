test_that("interpolation data looks ok", {
  grid <- create_grid()
  march_2024_data <- daily_weather |> dplyr:::filter(LST_DATE >= "2024-03-01", LST_DATE < "2024-04-01") |> dplyr:::group_by(`station name`, LONGITUDE, LATITUDE) |> dplyr:::summarise(avg_temp = mean(T_DAILY_AVG))
  interp = interpolate_data(
    avg_temp ~ LONGITUDE + LATITUDE, data = march_2024_data, gridpoints = grid
  )
  expect_equal(colnames(interp),
               c("interpolations", "longitudes", "latitudes", "inUSA"))
  expect_equal(class(interp$interpolations), "numeric")
  expect_equal(nrow(interp), 2500)
})
