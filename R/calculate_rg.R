
#' Calculate the radius of gyration from FFF-MALS data
#'
#' @param data A data frame.
#' @param window The time interval on which to return calculated radii.
#' @param method Use method "zimm" or "watt".
#'
#' @return
#' @export
#' @importFrom dplyr %>%
#' @examples
calculate_rg <- function(data, window = .1, method = "zimm") {
  # mals calibration coefficients:
  mals_calib <- tibble::tribble(
    ~number, ~theta, ~coefficient,
    1,      7,  0.00178775,
    2,     12, 0.00368763,
    3,     20,  0.00271051,
    4,     28,  0.00176462,
    5,     36,  0.00231833,
    6,     44,  0.00152087,
    7,     52,  0.00185855,
    8,     60,  0.00194345,
    9,     68,  0.00194282,
    10,    76,  0.00206297,
    11,    84,  0.00215706,
    12,    90,  0.00248170,
    13,   100,  0.00213770,
    14,   108,  0.00192152,
    15,   116,  0.00199917,
    16,   124,  0.00181977,
    17,   132,  0.00161909,
    18,   140,  0.00135980,
    19,   148,  0.00175342,
    20,   156,  0.00158798,
    21,   164,  0.00202189
  )

  data %>%
    dplyr::mutate(
      theta = stringr::str_extract(param, "\\d+") %>% as.numeric(),
      theta_rad = pi * theta / 180, # convert to radians
      x = sin(theta_rad / 2) ^ 2
    ) %>%
    # MALS calibration coefficients
    dplyr::left_join(mals_calib, by = "theta") %>%
    dplyr::mutate(
      rayleigh_ratio = conc * coefficient,
      y = 1 / rayleigh_ratio
    ) %>%
    dplyr::group_by(date, sample, timeslice = plyr::round_any(time, window)) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      model = purrr::map(data, ~ if(method == "zimm") {stats::lm(y ~ x, data = .x)} else {stats::lm(rayleigh_ratio ~ x + I(x ^ 2), data = .x)}),
      coef = purrr::map(model, ~ stats::coef(.x)),
      # include the index of refraction of water (1.33)?
      # Many texts leave it out, but it does result in a match to the PostNova program output
      rg_zimm = purrr::map(coef,
        ~ if(method == "zimm") {
          suppressWarnings(sqrt(3 * 532 ^ 2 * (.x[2] / .x[1]) / (16 * pi ^ 2 * 1.33 ^ 2)))
        } else NA_real_
      ),
      # from https://doi.org/10.1007/s11051-018-4397-x
      rg_watt = purrr::map(coef,
        ~ if(method == "watt") {
          suppressWarnings(sqrt(3 * 532 ^ 2 * (-.x[2]/.x[1])/ (16 * pi ^ 2)))
        } else NA_real_
      ),
    ) %>%
    tidyr::unnest(c(tidyselect::starts_with("rg"), data)) %>%
    dplyr::select_if(~ !is.list(.x))
}
