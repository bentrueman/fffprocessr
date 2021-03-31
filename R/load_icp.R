
#' Load FFF-ICP files (with extension .csv)
#'
#' @param path The relative path to the ICP-MS (.csv) files.
#' @param calibrate Logical. Convert the raw instantaneous detector counts per second to instantaneous concentrations?
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
load_icp <- function(path, calibrate = TRUE) {

  calib <- if(calibrate) {
    list.files(path = path, pattern = "*.xlsx", full.names = TRUE) %>%
      rlang::set_names() %>%
      purrr::map_dfr(readxl::read_excel, .id = "file") %>%
      dplyr::mutate(date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date()) %>%
      dplyr::group_by(date, param = element) %>%
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
        conc = cps,
        time = Time / 6e4
      ) %>%
      dplyr::filter(!is.na(conc))
  } else {
    data %>%
      dplyr::left_join(calib, by = c("date", "param")) %>%
      dplyr::mutate(
        conc = coef * cps,
        time = Time / 6e4
      ) %>%
      dplyr::filter(!is.na(conc))
  }

  out %>%
    dplyr::select(file, date, param, time, conc)

}
