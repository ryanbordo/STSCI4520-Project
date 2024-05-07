test_that( "temperature trend data looks ok", {

  ithaca_trend <- temperature_trend(64758)
  expect_equal(
    round(ithaca_trend$coefficients["years_elapsed", "Estimate"], 3),
    0.054
  )
  expect_equal(
    class(ithaca_trend),
    "summary.lm"
  )
})
