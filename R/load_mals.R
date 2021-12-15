
#' Load FFF-MALS data files with extension .txt
#'
#' @param path The relative path to the .txt input files.
#' @param angles MALS detector angles.
#' @param date_regex An optional regular expression for extracting dates from filenames.
#' @param date_format An optional non-standard date format corresponding to the output of
#' `date_regex` (see `?strptime`).
#' @param keywords An optional vector of pattern matches to pass to `stringr::str_detect()` that tell `load_mals()`
#' which files to load. These can be regular expressions.
#' @param ... Other arguments passed on to `read_table()`
#' @param angle_names A regular expression specifying the naming convention for MALS data files. The default is `ls\\d+-\\d+`,
#' or "ls" followed by the range of angles the file contains (e.g., "ls7-20").
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
  angles = fffprocessr:::angles,
  date_regex = "\\d{4}-\\d{2}-\\d{2}", date_format = "%Y-%m-%d",
  keywords = NULL,
  angle_names = "ls\\d+-\\d+",
  ...
) {

  angles <- suppressMessages(
    tibble::as_tibble(matrix(angles, byrow = TRUE, nrow = 7), .name_repair = "unique")
  ) %>%
    dplyr::rename_all(~ paste0("v", stringr::str_extract(.x, "\\d+")))

  file_list <- list.files(path = path, pattern = ".+\\.txt", full.names = TRUE)

  file_list <- file_list[stringr::str_detect(file_list, angle_names)]

  if(length(file_list) == 0) stop(
    "No filenames match the naming convention for MALS data.
    Use 'angle_names=?' to specify a non-default naming scheme."
  )

  keep_files <- if(is.null(keywords)) {rep(TRUE, length(file_list))} else{
    stringr::str_detect(file_list, paste(keywords, collapse = "|"))
  }

  args <- list(...)

 tibble::tibble(file = file_list) %>%
   dplyr::filter(keep_files) %>%
    # extract first of three consecutive angles in each file:
    dplyr::mutate(
      v1 = stringr::str_replace(file, "(.+)(_ls)(\\d+)(.+)", "\\3") %>%
        as.numeric()
    ) %>%
    dplyr::left_join(angles, by = "v1") %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("v")), ~ paste0("ls", .x)) %>%
    dplyr::mutate(
      data = purrr::map(file,
        ~ do.call(
          readr::read_table,
          c(args,
            list(
             .x,
             col_names = FALSE,
             col_types = readr::cols(.default = readr::col_character())
            )
          ),
        )
      ),
      renamed = purrr::pmap(
        list(.data$data, .data$v1, .data$v2, .data$v3),
        function(a, b, c, d) {names(a) <- c("time", b, "X3", c, "X5", d, "X7"); a}
      ),
      long = purrr::map(.data$renamed,
        ~ dplyr::select_at(.x, dplyr::vars(-tidyselect::matches("^X\\d$"))) %>%
          tidyr::pivot_longer(-.data$time, names_to = "param", values_to = "conc")
      )
    ) %>%
    tidyr::unnest(.data$long) %>%
    dplyr::mutate(
      date = stringr::str_extract(file, date_regex) %>% as.Date(date_format),
      sample = stringr::str_replace(
        file, "(.+)(\\d{4}-\\d{2}-\\d{2}[_-])(.+)(_ls.+)(\\.[:alpha:]+)", "\\3"
      ),
      time = as.numeric(.data$time),
      conc = as.numeric(.data$conc)
    ) %>%
    dplyr::select(file, date, sample, .data$param, .data$time, .data$conc)
}
