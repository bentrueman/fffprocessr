
#' Linear baseline correction with left and right endpoints
#'
#' @param x A tibble of the type returned by `load_icp()`.
#' @param left The left endpoint.
#' @param right The right endpoint.
#' @param window The fitting window at each endpoint.
#' @param group_vars Grouping variables for input to `dplyr::group_by()`. Set to NULL when there are no grouping variables.
#' @param x_var Name of x variable.
#' @param y_var Name of y variable.
#'
#' @return A tibble of the type returned by `load_icp()`, with the column 'conc' modified
#' by a linear baseline correction.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
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
  group_vars = c("date", "sample", "param"),
  x_var = "time",
  y_var = "conc"
) {

  input <- if(is.null(group_vars)) {
    x %>% tidyr::nest(data = tidyselect::everything())
  } else {
    x %>%
      dplyr::group_by(!!!rlang::syms(group_vars)) %>%
      tidyr::nest() %>%
      dplyr::ungroup()
  }

  formula_lm <- glue::glue("{y_var} ~ {x_var}")

  input %>%
    dplyr::mutate(
      # next lines apply a linear baseline correction to the data:
      subset = purrr::map(
        .data$data,
        ~ .x %>%
          dplyr::filter(abs(.data[[x_var]] - left) < window | abs(.data[[x_var]] - right) < window)
      ),
      model = purrr::map(.data$subset, ~ stats::lm(stats::as.formula(formula_lm), data = .x)),
      baseline = purrr::map2(.data$model, .data$data, ~ stats::predict(.x, newdata = .y)),
      corr = purrr::map2(.data$data, .data$baseline, ~ dplyr::pull(.x, .data[[y_var]]) - .y)
    ) %>%
    tidyr::unnest(c(.data$data, .data$corr)) %>%
    dplyr::select_if(~ !is.list(.x)) %>%
    dplyr::select(-.data[[y_var]]) %>%
    dplyr::rename(!!y_var := .data$corr)
}
