test_that("load_direct_quant() yields correct concentrations", {
  path <- system.file("extdata", package = "fffprocessr")
  file <- load_direct_quant(path = paste0(path, "/direct_quantification"))
  expect_equal(mean(file$value, na.rm = TRUE), 660.61344)
})
