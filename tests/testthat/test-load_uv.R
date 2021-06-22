path <- system.file("extdata", package = "fffprocessr")

test_that("load_uv() example yields expected number of rows", {
  expect_equal(nrow(load_uv(path = path, UV254_1, UV254_2, LS90)),  9363)
})

test_that("load_icp() yields expected column means", {
  expect_equal(
    dplyr::summarize_if(
      load_uv(path = path, UV254_1, UV254_2, LS90),
      is.numeric, ~ round(mean(.x), 1)
    ),
    tibble::tibble(time = 19, conc = .1)
  )
})
