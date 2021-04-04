
#' Load FFF-MALS data files with extension .txt
#'
#' @param path The relative path to the .txt input files.
#' @param angles MALS detector angles.
#'
#' @return A tibble with the columns 'file', 'date', 'sample', 'param', 'time', and 'conc'.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' path <- system.file("extdata/mals", package = "fffprocessr")
#' load_mals(path = path)
load_mals <- function(
  path,
  angles = c(7, 12, 20,  28, 36, 44, 52, 60, 68, 76, 84,
    90, 100, 108, 116, 124, 132, 140, 148, 156, 164)
) {

  angles <- suppressMessages(
    tibble::as_tibble(matrix(angles, byrow = TRUE, nrow = 7), .name_repair = "unique")
  ) %>%
    dplyr::rename_all(~ paste0("v", stringr::str_extract(.x, "\\d+")))

 tibble::tibble(file = list.files(path = path, pattern = ".+\\.txt", full.names = TRUE)) %>%
    # extract first of three consecutive angles in each file:
    dplyr::mutate(v1 = stringr::str_replace(file, "(.+)(_ls)(\\d+)(.+)", "\\3") %>% as.numeric()) %>%
    dplyr::left_join(angles, by = "v1") %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("v")), ~ paste0("ls", .x)) %>%
    dplyr::mutate(
      data = purrr::map(file, ~ readr::read_table2(.x, col_names = FALSE)),
      renamed = purrr::pmap(
        list(.data$data, .data$v1, .data$v2, .data$v3),
        function(a, b, c, d) {names(a) <- c("time", b, "X3", c, "X5", d, "X7"); a}
      ),
      long = purrr::map(.data$renamed,
        ~ dplyr::select_at(.x, dplyr::vars(-tidyselect::starts_with("X"))) %>%
          tidyr::pivot_longer(-.data$time, names_to = "param", values_to = "conc")
      )
    ) %>%
    tidyr::unnest(.data$long) %>%
    dplyr::mutate(
      date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date(),
      sample = stringr::str_replace(file, "(.+)(\\d{4}-\\d{2}-\\d{2}_)(.+)(_ls.+)(\\.[:alpha:]+)", "\\3")
    ) %>%
    dplyr::select(file, date, sample, .data$param, .data$time, .data$conc)
}
