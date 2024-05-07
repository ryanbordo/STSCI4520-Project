test_that( "grid generation looks ok", {

  grid <- create_grid()
  expect_equal(
    length(grid),
    2
  )
  expect_equal(
    all(class(grid) == c("sf", "data.frame")),
    TRUE
  )
  expect_equal(
    length(grid$geometry),
    2500
  )
})
