
test_that("integrate_peak() yields expected sum", {
  peak_fun <- function(bound) {
    x <- seq(-bound, bound, by = 1e-3)
    y <- dnorm(x)
    integrate_peak(x, y)
  }
  expect_equal(peak_fun(1), .682, tolerance = 1e-2)
  expect_equal(peak_fun(2), .954, tolerance = 1e-2)
  expect_equal(peak_fun(3), .997, tolerance = 1e-2)
})
