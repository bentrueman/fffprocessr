
test_that("correct_baseline() works with group_vars = NULL and alternate x and y variable names", {
  data <- help_correct_baseline()
  expect_equal(
    ncol(correct_baseline(
      data, left = .2, right = 29.8,
      group_vars = NULL, x_var = "test_x", y_var = "test_y"
    )),
    3
  )
})

test_that("correct_baseline() works with alternate groupng variables and x/y variable names", {
  data <- help_correct_baseline()
  expect_equal(
    ncol(correct_baseline(
      data, left = .2, right = 29.8,
      group_vars = "x", x_var = "test_x", y_var = "test_y"
    )),
    3
  )
})
