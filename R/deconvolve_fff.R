
#' Deconvolve incompletely resolved peaks
#'
#' @param x A vector of retention times.
#' @param y A vector of detector responses.
#' @param h A vector of guesses for peak height, with length equal to the desired number of peaks.
#' @param mu A vector of guesses for peak means, with length equal to the desired number of peaks.
#' @param s A vector of guesses for peak standard deviations, with length equal to the desired number of peaks.
#' @param g A vector of guesses for peak shape parameters, with length equal to the desired number of peaks. Silently
#' ignored if `fn == "normal`.
#' @param fn Function for peak fitting: Gaussian, skew Gaussian or exponentially modified Gaussian ("normal",
#' "skew_gaussian" or "emg").
#' @param algorithm Refer to nls() in the 'stats' package.
#' @param iter Refer to nls() in the 'stats' package.
#' @param tol Refer to nls() in the 'stats' package.
#'
#' @return A list containing the nls() model, the fitted model evaluated at x, and a tibble with a
#' column for each of the component peaks evaluated at x.
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- seq(0, 10, length.out = 100)
#' # two gaussian peaks:
#' y <- exp(-(x - 3) ^ 2 / 2) + exp(-(x - 7) ^ 2 / 2) + rnorm(100, 0, .1)
#' deconvolve_fff(x, y, h = c(1, 1), mu = c(3, 7), s = c(1, 1), fn = "normal")
deconvolve_fff <- function(
  x, y, h, mu, s, g = NULL,
  fn = "skew_gaussian",
  algorithm = "port",
  iter = 300, tol = 1e-1
) {

  peaks <- if(fn == "normal") list(h, mu, s) else list(h, mu, s, g)
  peaks <- peaks %>%
    purrr::map_dbl(length) %>%
    unique()

  if(length(peaks) > 1) stop("h, mu, s, and g must be equal in length.")

  if(sum(fn %in% c("skew_gaussian", "emg", "normal")) == 0) stop(
    "specify a valid function (emg or skew_gaussian)."
  )

  inputs <- if(fn == "normal") c("h", "mu", "s") else c("h", "mu", "s", "g")

  start_vals <- if(fn == "normal") {
    as.list(c(rename_inputs(h), rename_inputs(mu), rename_inputs(s)))
  } else {
    as.list(c(rename_inputs(h), rename_inputs(mu), rename_inputs(s), rename_inputs(g)))
  }

  formula <- stats::reformulate(
    termlabels = purrr::map_chr(
      seq_len(peaks),
      ~ paste0(paste0(paste0(fn, "("), c_inputs(inputs, .x)), ")")
    ),
    response = "y"
  )

  model <- stats::nls(
    formula = formula,
    start = start_vals,
    algorithm = algorithm,
    control = stats::nls.control(maxiter = iter, tol = tol)
  )

  coefs <- stats::coef(model)

  peaks <- purrr::map(seq_len(peaks), ~ component_peaks(x, coefs, peak = .x, fn))
  peaks <- rlang::set_names(peaks, nm = paste0("peak", seq_len(length(peaks)))) %>%
    dplyr::bind_cols()

  list("model" = model, "fitted" = stats::predict(model), "peaks" = peaks)

}

# error function, used in skew_gaussian:
error_fun <- function(x) {
  2 * stats::pnorm(x * sqrt(2)) - 1
}

# complementary error function:
erfc <- function(x) {
  1 - error_fun(x)
}

# for nls formula:
normal <- function(x, h, mu, s) {
  h * exp(-(x - mu) ^ 2 / (2 * s ^ 2))
}

skew_gaussian <- function(x, h, mu, s, g) {
  h * exp(-(x - mu) ^ 2 / (2 * s ^ 2)) * (1 + error_fun(g * (x - mu) / (sqrt(2) * s)))
}

emg <- function(x, h, mu, s, g) {
  (h * s / g) * sqrt(pi / 2) * exp(.5 * (s / g) ^ 2 - (x - mu) / g) *
    erfc((1/ sqrt(2)) * (s / g - (x - mu) / s))
}

# for component peaks in output:
component_peaks <- function(x, coefs, peak = 1, fn) {

  coefs <- as.list(coefs[stringr::str_detect(names(coefs), as.character(peak))])

  if(fn == "skew_gaussian") {
    skew_gaussian(x, coefs$h, coefs$mu, coefs$s, coefs$g)
  } else if(fn == "emg") {
    emg(x, coefs$h, coefs$mu, coefs$s, coefs$g)
  } else if(fn == "normal") {
    normal(x, coefs$h, coefs$mu, coefs$s)
  }
}

# create a single input string:
c_inputs <- function(input, number) paste(c("x", paste0(input, number)), collapse = ", ")

rename_inputs <- function(x) {
  names(x) <- paste0(deparse(substitute(x)), seq_len(length(x)))
  x
}
