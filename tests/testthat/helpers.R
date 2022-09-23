
# test-correct_baseline.R

help_correct_baseline <- function() {
  data <- data.frame(
    x = "a",
    test_x = seq(0, 30, by = .1),
    test_y = stats::runif(301, 0, 1)
  )
  data$test_y <- data$test_y + data$test_x
  data
}

# test-peak_maxima.R

help_peak_maxima <- function() {

  path <- system.file("extdata", package = "fffprocessr")
  data <- combine_fff(load_icp(path))
  data <- data[data$param == "65Cu", ]

  tesdat <- tibble::tibble(
    x = "a",
    test_x = 1:100,
    test_y = withr::with_seed(101, {stats::rlnorm(length(x), sdlog = 3)})
  )

  list(data = data, tesdat = tesdat)

}
