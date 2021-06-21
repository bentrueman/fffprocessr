path <- system.file("extdata", package = "fffprocessr")

test_that("load_uv() example yields expected number of rows", {
  expect_equal(nrow(load_uv(path = path, UV254_1, UV254_2, LS90)),  9363)
})
