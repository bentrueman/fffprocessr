
path <- system.file("extdata/mals", package = "fffprocessr")
mals_data <- load_mals(path = path)

test_that("calculate_rg() returns expected results.", {
  rg <- calculate_rg(mals_data)
  expect_equal(range(rg$rg_zimm), c(29.833372, 85.880589))
})

test_that("calculate_rg() returns expected results for 'watt' method.", {
  mals_corr <- correct_baseline(mals_data, 4, 65)
  mals_corr <- mals_corr[mals_corr$param != "ls7", ]
  rg <- calculate_rg(mals_corr, method = "watt")
  rg_40 <- rg$rg_watt[rg$timeslice == 40]
  expect_equal(round(unique(rg_40), 4), 135.1861)
})


