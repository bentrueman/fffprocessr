
#' Linear baseline correction with left and right endpoints
#'
#' @param x A data frame.
#' @param left The left endpoint.
#' @param right The right endpoint.
#' @param window The fitting window at each endpoint.
#'
#' @return
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
correct_baseline <- function(x, left = 10, right = 35, window = .2) {
  x %>%
    dplyr::group_by(date, sample, .data$param) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
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
