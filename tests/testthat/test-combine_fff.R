
path <- system.file("extdata", package = "fffprocessr")
test_that("combine_fff() yields same number of rows as load_...() fns", {
  icp_data <- load_icp(path)
  uv_data <- load_uv(path, UV254_1, UV254_2, LS90)
  expect_equal(
    nrow(combine_fff(icp_data, uv_data)),
    sum(
      nrow(dplyr::filter(icp_data, sample != "blank")),
      nrow(dplyr::filter(uv_data, sample != "blank"))
    )
  )
})

test_that("combine_fff() yields expected column means", {
  expect_equal(
    dplyr::summarize_if(
      combine_fff(load_icp(path), load_uv(path, UV254_1, UV254_2, LS90)),
      is.numeric, ~ round(mean(.x), 1)
    ),
    tibble::tibble(time = 19, conc = 1.1, three_sigma = .5)
  )
})

test_that("combine_fff() handles missing blank runs", {

  missing_blank <- tidyr::crossing(
    date = c("2021-01-01", "2021-02-01"),
    sample = c("blank", "sample_1"),
    param = letters[1:2],
    time = 1:2
  ) %>%
    tidyr::unite(file, date, sample, param, remove = FALSE) %>%
    dplyr::mutate(conc = runif(nrow(.))) %>%
    dplyr::filter(!(sample == "blank" & date == "2021-02-01"))

  expect_equal(
    nrow(
      dplyr::distinct(
        combine_fff(uv = missing_blank, subtract_blank = TRUE, focus = 1),
        date, param
      )
    ), 4
  )
})
