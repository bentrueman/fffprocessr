path <- system.file("extdata", package = "fffprocessr")

test_that("load_icp() example yields expected number of rows", {
  expect_equal(nrow(load_icp(path = path)),  8445)
})
