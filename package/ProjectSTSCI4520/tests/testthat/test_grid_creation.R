test_that( "temperature trend data looks ok", {

  grid <- create_grid()
  expect_equal(
    length(grid),
    2
  )
  expect_equal(
    all(class(gridpoints) == c("sf", "data.frame")),
    TRUE
  )
  expect_equal(
    length(gridpoints$geometry),
    2500
  )
})
