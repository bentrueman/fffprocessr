
#' Load FFF-UV-MALS data files with extension .txt
#'
#' @param path Relative path to the .txt input files.
#' @param nm1 Name of column 2 in the input files.
#' @param nm2 Name of column 4 in the input files.
#' @param nm3 Name of column 6 in the input files.
#' @param date_regex An optional regular expression for extracting dates from filenames.
#' @param date_format An optional non-standard date format corresponding to the output of
#' `date_regex` (see `?strptime`).
#' @param keywords An optional vector of pattern matches to pass to `stringr::str_detect()` that tell `load_uv()`
#' which files to load. These can be regular expressions.
#' @param ... Other arguments passed on to `read_table()`
#'
#' @return A tibble with the columns 'file', 'date', 'param', 'time', and 'conc'.
#' @importFrom rlang :=
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' path <- system.file("extdata", package = "fffprocessr")
#' load_uv(path = path, UV254_1, UV254_2, LS90)
load_uv <- function(
  path, nm1 = "X2", nm2 = "X4", nm3 = "X6",
  date_regex = "\\d{4}-\\d{2}-\\d{2}", date_format = "%Y-%m-%d",
  keywords = NULL,
  ...
) {

  file_list <- list.files(path = path, pattern = "*.txt", full.names = TRUE)

  keep_files <- if(is.null(keywords)) {rep(TRUE, length(file_list))} else{
    stringr::str_detect(file_list, paste(keywords, collapse = "|"))
  }

  mals_files <- stringr::str_detect(file_list[keep_files], "ls\\d+-\\d+")

  if(sum(mals_files) > 0) warning(
    "Some filenames include the default naming convention for MALS data (e.g., 'ls7-20')."
  )

  args <- list(...)

  file_list[keep_files] %>%
    rlang::set_names() %>%
    purrr::map_dfr(
      ~ do.call(readr::read_table,
        c(args,
          list(
            .x,
            col_names = FALSE,
            col_types = readr::cols(.default = readr::col_character())
          )
        )
      ),
      .id = "file"
    ) %>%
    dplyr::rename(
      time = .data$X1, {{nm1}} := .data$X2,
      {{nm2}} := .data$X4, {{nm3}} := .data$X6
    ) %>%
    # deselect unnamed columns:
    dplyr::select_at(dplyr::vars(-tidyselect::matches("^X\\d$"))) %>%
    dplyr::mutate(date = stringr::str_extract(file, date_regex) %>% as.Date(date_format)) %>%
    tidyr::pivot_longer(-c(file, date, .data$time), names_to = "param", values_to = "conc") %>%
    dplyr::mutate(
      time = as.numeric(.data$time), conc = as.numeric(.data$conc),
      # extract sample name:
      sample = stringr::str_replace(file, "(.+)(\\d{4}-\\d{2}-\\d{2}[_-])(.+)(\\.[:alpha:]+)", "\\3"),
      # rename samples with "blank" in the name:
      sample = dplyr::if_else(stringr::str_detect(sample, "[bB]lank"), "blank", sample),
      sample = dplyr::if_else(sample == "blank", sample, paste0("sample_", sample))
    ) %>%
    dplyr::select(file, .data$sample, date, .data$param, .data$time, .data$conc)
}
