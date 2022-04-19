test_that("calibrate_mw() yields the expected retention times", {
  retention <- c(11.35890, 19.49773, 19.24173, 13.88310, 18.73001, 17.67315)
  mw_kda <- c(0.69, 2000.00, 440.00, 1.80, 158.00, 44.00)
  ref <- c(13.257117, 16.411758, 18.154797, 19.513002)
  calculated <- calibrate_mw(retention, mw_kda, newdata = c(1, 10, 100, 1000), predict = TRUE)
  expect_equal(ref, calculated)
})
