
#' Load FFF-ICP data files with extension .csv
#'
#' @param path Relative path to the ICP-MS (.csv) input files.
#' @param calibrate Logical. Convert the raw instantaneous detector counts per second to
#' instantaneous concentrations? This requires a table for each date, in .xlsx format, with the
#' following columns: `file`, a character vector of filenames; `element`, a character vector
#' describing each calibration curve (e.g., `56Fe`); `defined`, a numeric vector of defined
#' standard concentrations; and `mean_cps` a numeric vector of mean counts per second at each
#' concentration
#' @param date_regex An optional regular expression for extracting non-ISO dates from filenames.
#' @param date_format An optional non-standard date format corresponding to the output of
#' `date_regex` (see `?strptime`).
#' @param calib_path Optional. Use if the relative path to the ICP-MS calibration files differs
#' from the relative path to the data files.
#' @param keywords An optional vector of pattern matches to pass to `stringr::str_detect()` that tell `load_icp()`
#' which files to load. These can be regular expressions.
#' @param data_format Selects an appropriate `readr` function based on the data format. Current
#' options are "x-series II" and "iCAP-RQ".
#'
#'
#' @return A tibble with the columns 'file', 'date', 'param', 'time', and 'conc'.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' path <- system.file("extdata", package = "fffprocessr")
#' load_icp(path = path)
load_icp <- function(
  path,
  calibrate = TRUE,
  date_regex = "\\d{4}-\\d{2}-\\d{2}",
  date_format = "%Y-%m-%d",
  calib_path = NULL,
  keywords = NULL,
  data_format = "x-series II"
) {

  # calibration:

  if(is.null(calib_path)) calib_path <- path

  calib_file_list <- list.files(path = calib_path, pattern = "*.xlsx", full.names = TRUE)

  if(calibrate & length(calib_file_list) == 0) stop("No calibration files found.")

  calib <- load_calib(
    calibrate, data_format, date_regex,
    date_format, calib_file_list
  )

  # file list:

  file_list <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

  keep_files <- if(is.null(keywords)) {rep(TRUE, length(file_list))} else{
    stringr::str_detect(file_list, paste(keywords, collapse = "|"))
  }

  file_dates <- file_list[keep_files] %>%
    stringr::str_extract(date_regex) %>%
    as.Date(date_format)

  if(
    calibrate & sum(unique(calib$date) %in% unique(file_dates)) == 0
  ) stop(
    "No calibration file dates match data file dates"
  )

  # read data:

  data <- file_list[keep_files] %>%
    rlang::set_names() %>%
    purrr::map_dfr(~ read_fun(.x, data_format), .id = "file") %>%
    dplyr::mutate(
      date = stringr::str_extract(file, date_regex) %>%
        as.Date(date_format)
    )

  out <- if(is.null(calib)) {
    data %>%
      dplyr::mutate(conc = .data$cps) %>%
      dplyr::filter(!is.na(.data$conc))
  } else {
    data %>%
      dplyr::left_join(calib, by = c("date", "param")) %>%
      dplyr::mutate(conc = .data$coef * .data$cps) %>%
      dplyr::filter(!is.na(.data$conc))
  }

  out_clean <- out %>%
    dplyr::mutate(
      # extract sample name:
      sample = stringr::str_replace(file, "(.+)(\\d{4}-\\d{2}-\\d{2}[_-])(.+)(\\.[:alpha:]+)", "\\3"),
      # rename samples with "blank" in the name:
      sample = dplyr::if_else(stringr::str_detect(sample, "[bB]lank"), "blank", sample),
      sample = dplyr::if_else(sample == "blank", sample, paste0("sample_", sample))
    ) %>%
    dplyr::select(
      file, .data$sample, date, .data$param,
      .data$time, .data$conc
    )

  out_clean

}

load_calib <- function(
  calibrate,
  data_format,
  date_regex,
  date_format,
  calib_file_list
) {
  if(calibrate & data_format == "x-series II") {
    calib_file_list %>%
      rlang::set_names() %>%
      purrr::map_dfr(readxl::read_excel, .id = "file") %>%
      dplyr::mutate(date = stringr::str_extract(file, date_regex) %>% as.Date(date_format)) %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::group_by(date, param = .data$element) %>%
      dplyr::summarize(coef = stats::lm(defined ~ 0 + mean_cps) %>% stats::coef()) %>%
      dplyr::ungroup()
  } else if(calibrate & data_format == "iCAP-RQ") {
    calib_file_list %>%
      rlang::set_names() %>%
      purrr::map_dfr(readxl::read_excel, .id = "file") %>%
      dplyr::mutate(date = stringr::str_extract(file, date_regex) %>% as.Date(date_format)) %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::mutate(run = stringr::str_extract(.data$Label, "^Wash|^Blank|^Standard.+|^QC.+")) %>%
      tidyr::fill(.data$run) %>%
      dplyr::filter(
        .data$Label == "Mean:",
        !stringr::str_detect(.data$run, "^QC|^Wash")
      ) %>%
      tidyr::pivot_longer(
        cols = tidyselect::matches("^\\d"),
        names_to = "param",
        values_to = "mean_cps",
        # some files may have different elements than others.
        # convert these explicit NAs to implicit NAs:
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(
        param = stringr::str_extract(.data$param, "\\d+[A-Z][a-z]?"),
        defined = stringr::str_replace(.data$run, ".+\\s(\\d+)ppb", "\\1") %>%
          stringr::str_replace("Blank", "0") %>%
          as.numeric(),
        mean_cps = stringr::str_remove_all(.data$mean_cps, ",") %>% as.numeric()
      ) %>%
      dplyr::group_by(date, .data$param) %>%
      dplyr::summarize(
        coef = stats::lm(defined ~ 0 + mean_cps) %>%
          stats::coef()
      ) %>%
      dplyr::ungroup()
  } else NULL
}

read_fun <- function(x, data_format) {
  if(data_format == "x-series II") {
    metadata <- 1
    readr::read_csv(x, col_types = readr::cols(.default = readr::col_character())) %>%
      # remove metadata after column names but before data:
      dplyr::filter(!dplyr::row_number() %in% seq_len(metadata)) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::matches("^\\d")), as.numeric) %>%
      tidyr::pivot_longer(
        cols = tidyselect::matches("^\\d"),
        names_to = "param", values_to = "cps"
      ) %>%
      dplyr::mutate(time = as.numeric(.data$Time) / 6e4) # time in minutes
  } else if(data_format == "iCAP-RQ") {
    readr::read_delim(x, delim = ";", skip = 1) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::matches("^\\d")), as.numeric) %>%
      dplyr::rename_at(
        dplyr::vars(tidyselect::matches("^\\d")),
        ~ paste("cps", .x, sep = " ")
      ) %>%
      tidyr::pivot_longer(
        tidyselect::matches("^Time|^cps"),
        names_to = c(".value", "param"),
        names_sep = " "
      ) %>%
      dplyr::mutate(time = as.numeric(.data$Time) / 60) # time in minutes
  } else stop("Choose a valid data format ('x-series II' or 'iCAP-RQ')")
}
