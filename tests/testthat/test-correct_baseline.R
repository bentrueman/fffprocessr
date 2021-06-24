test_that("correct_baseline() works with group_vars = NULL", {

  data <- data.frame(
    time = seq(0, 30, by = .1),
    conc = stats::runif(301, 0, 1)
  )
  data$conc <- data$conc + data$time

  expect_equal(
    ncol(correct_baseline(data, left = .2, right = 29.8, group_vars = NULL)),
    2
  )
})
