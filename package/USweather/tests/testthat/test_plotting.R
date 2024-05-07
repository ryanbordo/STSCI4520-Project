test_that("plotting looks ok", {
  grid <- create_grid()
  march_2024_data <- daily_weather |> dplyr:::filter(LST_DATE >= "2024-03-01", LST_DATE < "2024-04-01") |> dplyr:::group_by(`station name`, LONGITUDE, LATITUDE) |> dplyr:::summarise(avg_temp = mean(T_DAILY_AVG))
  interp = interpolate_data(
    march_2024_data$avg_temp,
    march_2024_data$LONGITUDE,
    march_2024_data$LATITUDE,
    use_elev = F,
    grid
  )
  expect_equal(plot_interpolations(interp), NULL)
})
