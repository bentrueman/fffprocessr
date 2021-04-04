
#' Load FFF-UV-MALS data files with extension .txt
#'
#' @param path Relative path to the .txt input files.
#' @param nm1 Name of column 2 in the input files.
#' @param nm2 Name of column 4 in the input files.
#' @param nm3 Name of column 6 in the input files.
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
load_uv <- function(path, nm1 = "X2", nm2 = "X4", nm3 = "X6") {
  list.files(path = path, pattern = "*.txt", full.names = TRUE) %>%
    rlang::set_names() %>%
    purrr::map_dfr(~ readr::read_table2(.x, col_names = FALSE), .id = "file") %>%
    dplyr::rename(time = .data$X1, {{nm1}} := .data$X2, {{nm2}} := .data$X4, {{nm3}} := .data$X6) %>%
    dplyr::select_at(dplyr::vars(-tidyselect::starts_with("X"))) %>%
    dplyr::mutate(date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date()) %>%
    tidyr::pivot_longer(-c(file, date, .data$time), names_to = "param", values_to = "conc") %>%
    dplyr::filter(!stringr::str_detect(.data$param, "^X\\d$")) %>%  # remove unnamed parameters
    dplyr::select(file, date, .data$param, .data$time, .data$conc)
}
