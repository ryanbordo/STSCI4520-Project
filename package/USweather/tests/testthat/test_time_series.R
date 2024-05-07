test_that( "station time series data looks ok", {

  ithaca_weather <- get_station_weather(64758, start_date = "2010-01-01", end_date = "2020-01-01")
  expect_equal(
    colnames(ithaca_weather),
    c("WBANNO", "state", "station name", "LST_DATE", "CRX_VN", "LONGITUDE", "LATITUDE",
      "T_DAILY_MAX", "T_DAILY_MIN", "T_DAILY_MEAN", "T_DAILY_AVG", "P_DAILY_CALC",
      "SOLARAD_DAILY")
  )
  expect_equal(
    nrow(ithaca_weather),
    3653
  )
  expect_equal(
    class(ithaca_weather),
    "data.frame"
  )
})
