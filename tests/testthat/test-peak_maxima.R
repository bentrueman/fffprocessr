
test_that("peak_maxima() yields the expected peaks", {
  inputs <- help_peak_maxima()
  maxima <- peak_maxima(inputs$data, peaks = 3)
  expect_equal(sort(unique(round(maxima$time))), c(12, 14, 20, 21))
})

test_that("peak_maxima() methods equivalent for example data", {
  inputs <- help_peak_maxima()
  expect_equal(
    round(peak_maxima(inputs$data, peaks = 3, method = "gam")$time),
    round(peak_maxima(inputs$data, peaks = 3, method = "sigma", n = 7)$time)
  )
})

test_that("peak_maxima() works with group_vars = NULL and alternate x and y variable names", {
  inputs <- help_peak_maxima()
  result <- c(
    ncol(peak_maxima(inputs$tesdat, group_vars = NULL, x_var = "test_x", y_var = "test_y", method = "sigma")),
    ncol(peak_maxima(inputs$tesdat, group_vars = NULL, x_var = "test_x", y_var = "test_y", method = "gam"))
  )
  expect_equal(unique(result), 3)
})

test_that("peak_maxima() works with group_vars equal to an internal variable name", {
  inputs <- help_peak_maxima()
  expect_equal(
    ncol(peak_maxima(
      inputs$tesdat, group_vars = "x", x_var = "test_x", y_var = "test_y", method = "sigma"
    )),
    4
  )
})

test_that("peak_maxima() orders by peak height to reach the prespecified number of peaks", {
  inputs <- help_peak_maxima()
  tall_peaks <- c(
    peak_maxima(inputs$data, peaks = 1)$time,
    peak_maxima(inputs$data, peaks = 1, method = "sigma")$time
  )
  expect_equal(
    unique(round(tall_peaks)),
    14
  )
})

test_that("peak_maxima() handles NA", {
  inputs <- help_peak_maxima()
  inputs$tesdat$test_y[4] <- NA_real_
  expect_equal(
    ncol(peak_maxima(inputs$tesdat, group_vars = "x", x_var = "test_x", y_var = "test_y", method = "sigma")),
    4
  )
})



