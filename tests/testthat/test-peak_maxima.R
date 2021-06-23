
path <- system.file("extdata", package = "fffprocessr")
data <- combine_fff(load_icp(path))
data <- data[data$param == "65Cu", ]

test_that("peak_maxima() yields the expected peaks", {
  maxima <- peak_maxima(data, peaks = 3)
  expect_equal(unique(round(maxima$time)), c(12, 14, 20, 21))
})

test_that("peak_maxima() methods equivalent for example data", {
  expect_equal(
    round(peak_maxima(data, peaks = 3, method = "gam")$time),
    round(peak_maxima(data, peaks = 3, method = "sigma", n = 7)$time)
  )
})


