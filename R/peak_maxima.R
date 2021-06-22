
#' Detect peak maxima
#'
#' @param data A tibble returned by `load_icp()`, `load_uv()`, `load_mals()`, or `combine_fff()`
#' @param focus Focus period, in minutes.
#' @param k The basis dimension of the generalized additive model used to detect peak minima.
#' @param peaks Number of peaks.
#' @param n Before detecting peak minima, compute an n-point moving average of the detector response using
#' `stats::filter(x, filter = rep(1/n, n))`.
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
peak_maxima <- function(data, focus = 10, k = 35, peaks = 1, n = 1) {
  data %>%
    dplyr::filter(.data$time > focus) %>%
    dplyr::group_by(.data$date, .data$sample, .data$param) %>%
    dplyr::mutate(
      conc = stats::filter(.data$conc, rep(1/n, n)) %>%
        as.numeric() %>%
        imputeTS::na_interpolation(),
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

# sketch of an alternate method:

# # this groups each cluster:
#
# group_peaks <- function(x) {
#   x <- c(0, x) # pad x with a zero so that x[i-1] exists when i=1
#   g <- rep(NA_real_, length(x))
#   counter <- 0
#   for(i in 2:length(x)) {
#     if(x[i] == 0) {
#       g[i] <- 0
#     } else if(x[i] == 1 & x[i] == x[i-1]) {
#       g[i] <- counter
#     } else {
#       counter <- counter + 1
#       g[i] <- counter
#     }
#   }
#   g[-1]
# }
#
# icp %>%
#   correct_baseline(10, 37) %>%
#   filter(param == "56Fe", sample == "sample_bennery_raw") %>%
#   mutate(
#     sd = sd(conc),
#     peak = conc > 3 * sd,
#     sd = sd(conc[!peak]),
#     peak = conc > 3 * sd,
#     g = group_peaks(peak)
#   ) %>%
#   ggplot(aes(time, conc, col = as.numeric(peak))) +
#   geom_point(
#     data = function(x) x %>%
#       group_by(g) %>%
#       summarize(
#         time = time[which.max(conc)],
#         conc = max(conc)
#       ) %>%
#       filter(g != 0),
#     aes(time, conc), inherit.aes = FALSE
#   ) +
#   geom_line()
