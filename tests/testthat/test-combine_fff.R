
path <- system.file("extdata", package = "fffprocessr")
test_that("number of rows is as expected", {
  expect_equal(
    nrow(combine_fff(load_icp(path), load_uv(path, UV254_1))), 11872
  )
})
