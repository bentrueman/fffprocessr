
#' Detect peak maxima
#'
#' @param data A tibble returned by `load_icp()`, `load_uv()`, `load_mals()`, or `combine_fff()`
#' @param focus Focus period, in minutes.
#' @param k The basis dimension of the generalized additive model used to detect peak minima.
#' @param peaks Number of peaks.
#'
#' @return A tibble of peak retention times and the corresponding maxima in detector response.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' path <- system.file("extdata", package = "fffprocessr")
#' data <- combine_fff(load_icp(path))
#' data <- data[data$param == "65Cu", ]
#' peak_maxima(data, peaks = 3)
peak_maxima <- function(data, focus = 10, k = 35, peaks = 1) {
  data %>%
    dplyr::filter(.data$time > focus) %>%
    dplyr::group_by(.data$date, .data$sample, .data$param) %>%
    dplyr::mutate(
      fitted = mgcv::gam(.data$conc ~ s(.data$time, bs = "cs", k = k)) %>%
        mgcv::predict.gam(),
      diff = dplyr::lead(.data$fitted) - .data$fitted, # slope
      sign = sign(.data$diff), # sign of slope
      diff_sign = dplyr::lead(.data$sign) - .data$sign, # delta sign of slope
      peak = cumsum(.data$diff_sign > 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$peak %in% seq_len(peaks)) %>%
    dplyr::group_by(.data$date, .data$sample, .data$param, .data$peak) %>%
    dplyr::summarize(
      conc_tr = max(.data$conc),
      tr = .data$time[which.max(.data$conc)]
    ) %>%
    dplyr::ungroup()
}

