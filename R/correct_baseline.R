
#' Linear baseline correction with left and right endpoints
#'
#' @param x A data frame.
#' @param left The left endpoint.
#' @param right The right endpoint.
#' @param window The fitting window at each endpoint.
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
correct_baseline <- function(x, left = 10, right = 35, window = .2) {
  x %>%
    dplyr::group_by(date, sample, param) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # next four lines apply a linear baseline correction to the data:
      subset = purrr::map(data, ~ dplyr::filter(.x, abs(time - left) < window | abs(time - right) < window)),
      model = purrr::map(subset, ~ stats::lm(conc ~ time, data = .x)),
      baseline = purrr::map2(model, data, ~ stats::predict(.x, newdata = .y)),
      corr = purrr::map2(data, baseline, ~.x$conc - .y)
    ) %>%
    tidyr::unnest(c(data, corr)) %>%
    dplyr::select_if(~ !is.list(.x)) %>%
    dplyr::select(-conc) %>%
    dplyr::rename(conc = corr)
}
