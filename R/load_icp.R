
#' Load FFF-ICP data files with extension .csv
#'
#' @param path Relative path to the ICP-MS (.csv) input files.
#' @param calibrate Logical. Convert the raw instantaneous detector counts per second to
#' instantaneous concentrations?
#'
#' @return A tibble with the columns 'file', 'date', 'param', 'time', and 'conc'.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' path <- system.file("extdata", package = "fffprocessr")
#' load_icp(path = path)
load_icp <- function(path, calibrate = TRUE) {

  calib <- if(calibrate) {
    list.files(path = path, pattern = "*.xlsx", full.names = TRUE) %>%
      rlang::set_names() %>%
      purrr::map_dfr(readxl::read_excel, .id = "file") %>%
      dplyr::mutate(date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date()) %>%
      dplyr::group_by(date, param = .data$element) %>%
      dplyr::summarize(coef = stats::lm(defined ~ 0 + mean_cps) %>% stats::coef()) %>%
      dplyr::ungroup()
  } else NULL


  data <- list.files(path = path, pattern = "*.csv", full.names = TRUE) %>%
    rlang::set_names() %>%
    purrr::map_dfr(~ readr::read_csv(.x)[-1, ], .id = "file") %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("^\\d")), as.numeric) %>%
    tidyr::pivot_longer(cols = tidyselect::matches("^\\d"), names_to = "param", values_to = "cps") %>%
    dplyr::mutate(date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date())

  out <- if(is.null(calib)) {
    data %>%
      dplyr::mutate(
        conc = .data$cps,
        time = .data$Time / 6e4
      ) %>%
      dplyr::filter(!is.na(.data$conc))
  } else {
    data %>%
      dplyr::left_join(calib, by = c("date", "param")) %>%
      dplyr::mutate(
        conc = .data$coef * .data$cps,
        time = .data$Time / 6e4
      ) %>%
      dplyr::filter(!is.na(.data$conc))
  }

  out %>%
    dplyr::select(file, date, .data$param, .data$time, .data$conc)

}
