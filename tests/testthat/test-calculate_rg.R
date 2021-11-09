test_that("calculate_rg() returns expected results.", {
  path <- system.file("extdata/mals", package = "fffprocessr")
  mals_data <- load_mals(path = path)
  rg <- calculate_rg(mals_data)
  expect_equal(range(rg$rg_zimm), c(29.833372, 85.880589))
})
