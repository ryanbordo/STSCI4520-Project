test_that( "yearly cycle data looks ok", {

  ithaca_cycle_no_leap <- get_yearly_cycle(64758)
  expect_equal(
    all(colnames(ithaca_cycle_no_leap)) == c("day_of_year", "avg_temp"),
    TRUE
  )
  expect_equal(
    nrow(ithaca_cycle_no_leap),
    365
  )
  expect_equal(
    class(ithaca_cycle_no_leap),
    "data.frame"
  )

  ithaca_cycle_leap <- get_yearly_cycle(64758, drop_leapdays = F)
  expect_equal(
    all(colnames(ithaca_cycle_leap)) == c("day_of_year", "avg_temp"),
    TRUE
  )
  expect_equal(
    nrow(ithaca_cycle_leap),
    366
  )
  expect_equal(
    class(ithaca_cycle_leap),
    "data.frame"
  )
})
