
#' Linear baseline correction with left and right endpoints
#'
#' @param x A tibble of the type returned by `load_icp()`.
#' @param left The left endpoint.
#' @param right The right endpoint.
#' @param window The fitting window at each endpoint.
#' @param group_vars Grouping variables for input to `dplyr::group_by()`. Set to NULL when there are no grouping variables.
#'
#' @return A tibble of the type returned by `load_icp()`, with the column 'conc' modified
#' by a linear baseline correction.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data <- data.frame(
#'   date = as.Date("2021-01-01"),
#'   sample = "a",
#'   param = "56Fe",
#'   time = seq(0, 30, by = .1),
#'   conc = runif(301, 0, 1)
#' )
#' data$conc <- data$conc + data$time
#' correct_baseline(data, left = .2, right = 29.8)
correct_baseline <- function(
  x,
  left = 10,
  right = 35,
  window = .2,
  group_vars = c("date", "sample", "param")
) {

  input <- if(is.null(group_vars)) {
    x %>% tidyr::nest(data = tidyselect::everything())
  } else {
    x %>%
      dplyr::group_by(!!!rlang::syms(group_vars)) %>%
      tidyr::nest() %>%
      dplyr::ungroup()
  }

  input %>%
    dplyr::mutate(
      # next four lines apply a linear baseline correction to the data:
      subset = purrr::map(.data$data, ~ dplyr::filter(.x, abs(time - left) < window | abs(time - right) < window)),
      model = purrr::map(.data$subset, ~ stats::lm(conc ~ time, data = .x)),
      baseline = purrr::map2(.data$model, .data$data, ~ stats::predict(.x, newdata = .y)),
      corr = purrr::map2(.data$data, .data$baseline, ~.x$conc - .y)
    ) %>%
    tidyr::unnest(c(.data$data, .data$corr)) %>%
    dplyr::select_if(~ !is.list(.x)) %>%
    dplyr::select(-.data$conc) %>%
    dplyr::rename(conc = .data$corr)
}
