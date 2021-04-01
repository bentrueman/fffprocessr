
#' Load MALS data files (.txt)
#'
#' @param path The relative path to the MALS (.txt) files.
#' @param angles MALS detector angles
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
load_mals <- function(
  path,
  angles = c(7, 12, 20,  28, 36, 44, 52, 60, 68, 76, 84, 90, 100, 108, 116, 124, 132, 140, 148, 156, 164)
) {

  angles <- suppressMessages(
    tibble::as_tibble(matrix(angles, byrow = TRUE, nrow = 7), .name_repair = "unique")
  ) %>%
    dplyr::rename_all(~ stringr::str_extract(.x, "\\d+") %>% paste0("v", .))

  list.files(path = path, pattern = ".+\\.txt", full.names = TRUE) %>%
    tibble::tibble(file = .) %>%
    # extract first of three consecutive angles in each file:
    dplyr::mutate(v1 = stringr::str_replace(file, "(.+)(_ls)(\\d+)(.+)", "\\3") %>% as.numeric()) %>%
    dplyr::left_join(angles, by = "v1") %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("v")), ~ paste0("ls", .x)) %>%
    dplyr::mutate(
      data = purrr::map(file, ~ readr::read_table2(.x, col_names = FALSE)),
      renamed = purrr::pmap(list(data, v1, v2, v3), function(a, b, c, d) {names(a) <- c("time", b, "X3", c, "X5", d, "X7"); a}),
      long = purrr::map(renamed,
        ~ dplyr::select_at(.x, dplyr::vars(-tidyselect::starts_with("X"))) %>%
          tidyr::pivot_longer(-time, names_to = "param", values_to = "conc")
      )
    ) %>%
    tidyr::unnest(long) %>%
    dplyr::mutate(
      date = stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date(),
      sample = stringr::str_replace(file, "(.+)(\\d{4}-\\d{2}-\\d{2}_)(.+)(_ls.+)(\\.[:alpha:]+)", "\\3")
    ) %>%
    dplyr::select(file, date, sample, param, time, conc)
}
