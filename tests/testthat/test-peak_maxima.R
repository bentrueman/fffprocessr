test_that("peak_maxima() yields the expected peaks", {
  path <- system.file("extdata", package = "fffprocessr")
  data <- combine_fff(load_icp(path))
  data <- data[data$param == "65Cu", ]
  maxima <- peak_maxima(data, peaks = 3)
  expect_equal(unique(round(maxima$tr)), c(12, 14, 20, 21))
})
