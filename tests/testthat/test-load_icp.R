path <- system.file("extdata", package = "fffprocessr")

test_that("load_icp() example yields expected number of rows", {
  expect_equal(nrow(load_icp(path = path)),  8445)
})


test_that("load_icp() yields expected column means", {
  expect_equal(
    dplyr::summarize_if(load_icp(path = path), is.numeric, ~ round(mean(.x), 1)),
    tibble::tibble(time = 18.9, conc = 10)
  )
})
