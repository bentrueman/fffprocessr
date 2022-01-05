
#' Detect peak maxima
#'
#' @param data A tibble returned by `load_icp()`, `load_uv()`, `load_mals()`, or `combine_fff()`
#' @param focus Peaks occurring at x values less than or equal to this value are not considered. For FFF data, this is the focus period.
#' @param k The basis dimension of the generalized additive model used to detect peak minima.
#' @param peaks Number of peaks.
#' @param n Before detecting peak minima, compute an n-point moving average of the detector response using
#' `stats::filter(x, filter = rep(1/n, n))`.
#' @param method Algorithm for peak detection. Either "gam", which separates peaks by the minima between
#' them, or "sigma" which recursively calculates the standard deviation by eliminating values greater
#' than three standard deviations. N.B., method = "sigma" silently removes NAs from x and y.
#' @param max_iter Maximum iterations for method = "sigma".
#' @param group_vars Grouping variables for input to `dplyr::group_by()`.
#' @param x_var Name of x variable.
#' @param y_var Name of y variable.
#'
#' @return A tibble of peak retention times and the corresponding maxima in detector response.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
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
  group_vars = c("date", "sample", "param"),
  x_var = "time",
  y_var = "conc"
) {

  data <- if(is.null(group_vars)) data else {
    data %>%
      dplyr::group_by(!!!rlang::syms(group_vars))
  }

  data_smooth <- data %>%
    dplyr::mutate(
      conc = stats::filter(.data[[y_var]], rep(1/n, n)) %>%
        as.numeric() %>%
        imputeTS::na_interpolation()
    )

  peak_pos <- if(method == "gam") {
    peak_id_gam(data_smooth, focus, k, peaks, group_vars, x_var, y_var)
  } else
    if(method == "sigma") {

      input <- if(is.null(group_vars)) {

        data_smooth %>%
          tidyr::nest(data = tidyselect::everything())

      } else {

        data_smooth %>%
          tidyr::nest() %>%
          dplyr::ungroup()
      }

      input %>%
        dplyr::mutate(
          peaks = purrr::map(.data$data, ~ peak_id_sigma(.x, focus, peaks, max_iter, x_var, y_var))
        ) %>%
        tidyr::unnest(.data$peaks) %>%
        dplyr::select(-.data$data)

    } else stop("choose a valid method: 'gam' or 'sigma'")

  # replace smoothed peak heights with unsmoothed peak heights:

  peak_pos %>%
    dplyr::left_join(
      data, by = tidyselect::all_of(c(group_vars, x_var)),
      suffix = c("", "_smooth")
    ) %>%
    dplyr::select(tidyselect::all_of(c(group_vars, "peak", x_var, y_var)))

}

peak_id_gam <- function(data, focus, k, peaks, group_vars, x_var, y_var) {

  peak_index <- seq_len(peaks)

  formula_gam <- glue::glue("{y_var} ~ s({x_var}, bs = 'cs', k = {k})")

  data %>%
    dplyr::filter(.data[[x_var]] > focus) %>%
    dplyr::mutate(
      fitted = mgcv::gam(stats::as.formula(formula_gam)) %>%
        mgcv::predict.gam(),
      diff = dplyr::lead(.data$fitted) - .data$fitted, # slope
      sign = sign(.data$diff), # sign of slope
      diff_sign = dplyr::lead(.data$sign) - .data$sign, # delta sign of slope
      peak = cumsum(.data$diff_sign > 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(c(group_vars, "peak"))) %>%
    dplyr::summarize(
      !!x_var := (.data[[x_var]])[which.max(.data[[y_var]])],
      !!y_var := max(.data[[y_var]]),
    ) %>%
    dplyr::ungroup() %>%
    # select number of peaks, starting with largest
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::arrange(dplyr::desc(.data[[y_var]])) %>%
    dplyr::slice(peak_index) %>%
    dplyr::mutate(peak = peak_index) %>%
    dplyr::ungroup()
}

peak_id_sigma <- function(data, focus, peaks, max_iter, x_var, y_var) {

  peak_index <- seq_len(peaks)

   # remove NAs:

  data <- data %>%
    dplyr::filter(dplyr::if_all(
      .cols = tidyselect::matches(tidyselect::all_of(c(x_var, y_var))),
      .fns = function(x) !is.na(x)
    ))

  peak_tbl <- tibble::tibble()
  peak_list <- list()
  peak_region <- rep(FALSE, nrow(data))
  iter <- 0
  peak_id <- 0

  while(peak_id < peaks & iter < max_iter) {

    iter <- iter + 1

    sdev <- data %>%
      dplyr::filter(!peak_region) %>%
      dplyr::pull(.data[[y_var]]) %>%
      stats::sd() %>%
      tidyr::replace_na(0)

    data <- data %>%
      dplyr::mutate(
        peak_region = .data[[y_var]] > 3 * sdev,
        g = group_peaks(peak_region)
      )

    peak_region <- data$peak_region

    peak_list[[iter]] <- list(
      data %>%
        dplyr::group_by(.data$g) %>%
        dplyr::summarize(
          !!x_var := (.data[[x_var]])[which.max(.data[[y_var]])],
          !!y_var := max(.data[[y_var]]),
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$g != 0)
    )

    peak_tbl <- dplyr::bind_rows(peak_list) %>%
      dplyr::bind_rows(peak_tbl) %>%
      dplyr::select(-.data$g) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::desc(.data[[y_var]])) %>%
      dplyr::filter(.data[[x_var]] > focus)

    peak_id <- nrow(peak_tbl)
  }

  peak_tbl %>%
    dplyr::slice(peak_index) %>%
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
