
#' Detect peak maxima
#'
#' @param data A tibble returned by `load_icp()`, `load_uv()`, `load_mals()`, or `combine_fff()`
#' @param focus Focus period, in minutes.
#' @param k The basis dimension of the generalized additive model used to detect peak minima.
#' @param peaks Number of peaks.
#' @param n Before detecting peak minima, compute an n-point moving average of the detector response using
#' `stats::filter(x, filter = rep(1/n, n))`.
#' @param method Algorithm for peak detection. Either "gam", which separates peaks by the minima between
#' them, or "sigma" which recursively calculates the standard deviation by eliminating values greater
#' than three standard deviations.
#' @param max_iter Maximum iterations for method = "sigma".
#' @param group_vars Grouping variables for input to `dplyr::group_by()`.
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
peak_maxima <- function(
  data,
  focus = 10,
  k = 35,
  peaks = 1,
  n = 1,
  method = "gam",
  max_iter = 20,
  group_vars = c("date", "sample", "param")
) {

  data <- data %>%
    dplyr::group_by(!!!rlang::syms(group_vars))

  data_smooth <- data %>%
    dplyr::mutate(
      conc = stats::filter(.data$conc, rep(1/n, n)) %>%
        as.numeric() %>%
        imputeTS::na_interpolation()
    )

  peak_pos <- if(method == "gam") peak_id_gam(data_smooth, focus, k, peaks, group_vars) else
    if(method == "sigma") {
      data_smooth %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(peaks = purrr::map(.data$data, ~ peak_id_sigma(.x, focus, peaks, max_iter))) %>%
        tidyr::unnest(.data$peaks) %>%
        dplyr::select(-.data$data)
    } else stop("choose a valid method: 'gam' or 'sigma'")

  # replace smoothed peak heights with unsmoothed peak heights:

  peak_pos %>%
    dplyr::left_join(data, by = c(group_vars, "time"), suffix = c("_smooth", "")) %>%
    dplyr::select(c(group_vars, "peak", "time", "conc"))

}

peak_id_gam <- function(data, focus, k, peaks, group_vars) {
  data %>%
    dplyr::filter(.data$time > focus) %>%
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
    dplyr::group_by(!!!rlang::syms(c(group_vars, "peak"))) %>%
    dplyr::summarize(
      time = .data$time[which.max(.data$conc)],
      conc = max(.data$conc),
    ) %>%
    dplyr::ungroup()
}

peak_id_sigma <- function(data, focus, peaks, max_iter) {

  peak_tbl <- tibble::tibble()
  peak_list <- list()
  peak_region <- rep(FALSE, nrow(data))
  iter <- 0
  peak_id <- 0

  while(peak_id < peaks & iter < max_iter) {

    iter <- iter + 1

    sdev <- tidyr::replace_na(stats::sd(data$conc[!peak_region]), 0)

    data <- data %>%
      dplyr::mutate(
        peak_region = .data$conc > 3 * sdev,
        g = group_peaks(peak_region)
      )

    peak_region <- data$peak_region

    peak_list[[iter]] <- list(
      data %>%
        dplyr::group_by(.data$g) %>%
        dplyr::summarize(
          time = .data$time[which.max(.data$conc)],
          conc = max(.data$conc),
        ) %>%
        dplyr::filter(.data$g != 0)
    )

    peak_tbl <- dplyr::bind_rows(peak_list) %>%
      dplyr::bind_rows(peak_tbl) %>%
      dplyr::select(-.data$g) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$time) %>%
      dplyr::filter(.data$time > focus)

    peak_id <- nrow(peak_tbl)
  }

  peak_tbl %>%
    dplyr::slice(1:peaks) %>%
    tibble::rowid_to_column(var = "peak")

}

group_peaks <- function(x) {

  x <- c(0, x) # pad x with a zero so that x[i-1] exists when i=1
  g <- rep(NA_real_, length(x))
  counter <- 0

  for(i in 2:length(x)) {
    if(x[i] == 0) {
      g[i] <- 0
    } else if(x[i] == 1 & x[i] == x[i-1]) {
      g[i] <- counter
    } else {
      counter <- counter + 1
      g[i] <- counter
    }
  }

  g[-1]
}
