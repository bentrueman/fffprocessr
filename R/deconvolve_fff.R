
#' Deconvolve incompletely resolved peaks
#'
#' @param x A vector of retention times.
#' @param y A vector of detector responses.
#' @param h A vector of guesses for peak height, with length equal to the desired number of peaks.
#' @param mu A vector of guesses for peak means, with length equal to the desired number of peaks.
#' @param s A vector of guesses for peak standard deviations, with length equal to the desired number of peaks.
#' @param g A vector of guesses for peak shape parameters, with length equal to the desired number of peaks.
#' @param fn Function for peak fitting: skew Gaussian or exponentially modified Gaussian ("skew_gaussian" or "emg").
#' @param algorithm Refer to nls() in the stats:: package.
#' @param iter Refer to nls() in the stats:: package.
#' @param tol Refer to nls() in the stats:: package.
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
deconvolve_fff <- function(
  x, y, h, mu, s, g,
  fn = "skew_gaussian",
  algorithm = "port",
  iter = 300, tol = 1e-1
) {

  peaks <- list(h, mu, s, g) %>%
    purrr::map_dbl(length) %>%
    unique()

  if(length(peaks) > 1) stop("h, mu, s, and g must be equal in length.")

  if(fn != "skew_gaussian" & fn != "emg") stop("specify a valid function (emg or skew_gaussian).")

  # error function, used in skew_gaussian:
  error_fun <- function(x) {
    2 * stats::pnorm(x * sqrt(2)) - 1
  }

  # complementary error function:
  erfc <- function(x) {
    1 - error_fun(x)
  }

  # for nls formula:
  skew_gaussian <- function(x, h, mu, s, g) {
    h * exp(-(x - mu) ^ 2 / (2 * s ^ 2)) * (1 + error_fun(g * (x - mu) / (sqrt(2) * s)))
  }

  emg <- function(x, h, mu, s, g) {
    (h * s / g) * sqrt(pi / 2) * exp(.5 * (s / g) ^ 2 - (x - mu) / g) *
      erfc((1/ sqrt(2)) * (s / g - (x - mu) / s))
  }

  # for component peaks in output:
  component_peaks <- function(x, coefs, peak = 1) {

    coefs <- as.list(coefs[stringr::str_detect(names(coefs), as.character(peak))])

    if(fn == "skew_gaussian") {
      skew_gaussian(x, coefs$h, coefs$mu, coefs$s, coefs$g)
    } else if(fn == "emg") {
      emg(x, coefs$h, coefs$mu, coefs$s, coefs$g)
    }

  }

  inputs <- c("h", "mu", "s", "g")

  c_inputs <- function(input, number) paste(c("x", paste0(input, number)), collapse = ", ")

  rename_inputs <- function(x) {
    names(x) <- paste0(deparse(substitute(x)), seq_len(length(x)))
    x
  }

  start_vals <- as.list(c(rename_inputs(h), rename_inputs(mu), rename_inputs(s), rename_inputs(g)))

  formula <- stats::reformulate(
    termlabels = purrr::map_chr(seq_len(peaks), ~ paste0(paste0(paste0(fn, "("), c_inputs(inputs, .x)), ")")),
    response = "y"
  )

  model <- stats::nls(
    formula = formula,
    start = start_vals,
    algorithm = algorithm,
    control = stats::nls.control(maxiter = iter, tol = tol)
  )

  coefs <- stats::coef(model)

  peaks <- purrr::map(seq_len(peaks), ~ component_peaks(x, coefs, peak = .x)) %>%
    rlang::set_names(nm = seq_len(length(.)) %>% paste0("peak", .)) %>%
    dplyr::bind_cols()

  list("model" = model, "fitted" = stats::predict(model), "peaks" = peaks)

}
