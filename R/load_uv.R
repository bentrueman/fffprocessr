
#' Load FFF-UV/Vis files (.txt)
#'
#' @param path The relative path to the UV (.txt) files.
#' @param nm1 Name of column 2.
#' @param nm2 Name of column 4.
#' @param nm3 Name of column 6.
#'
#' @return
#' @importFrom rlang :=
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
load_uv <- function(path, nm1 = X2, nm2 = X4, nm3 = X6) {
  list.files(path = path, pattern = "*.txt", full.names = TRUE) %>%
    rlang::set_names() %>%
    purrr::map_dfr(~ readr::read_table2(.x, col_names = FALSE), .id = "file") %>%
    dplyr::rename(time = X1, {{nm1}} := X2, {{nm2}} := X4, {{nm3}} := X6) %>%
    dplyr::select_at(dplyr::vars(-tidyselect::starts_with("X"))) %>%
    dplyr::mutate(date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date()) %>%
    tidyr::pivot_longer(-c(file, date, time), names_to = "param", values_to = "conc") %>%
    dplyr::filter(!stringr::str_detect(param, "^X\\d$")) %>%  # remove unnamed parameters
    dplyr::select(file, date, time, param, conc)
}
