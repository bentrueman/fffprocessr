test_that("load_mals() yields the expected error message when no files match the naming convention.", {
  path <- system.file("extdata", package = "fffprocessr") # wrong path
  expect_error(
    load_mals(path = path),
    "No filenames"
  )
})
