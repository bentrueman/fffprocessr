
#' Combine loaded UV-MALS and ICP-MS data
#'
#' @param icp A tibble created by `load_icp()`, or
#' @param uv A tibble created by `load_uv()`, or both.
#' @param subtract_blank Logical. Subtract a blank from the sample runs?
#' @param focus Focusing period, in minutes.
#'
#' @return A tibble with the columns date', 'sample', 'param', 'time', 'conc', and 'three_sigma'.
#' The final column, 'three_sigma' represents three times the standard deviation of blank runs
#' corresponding to each date and analyte.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' path <- system.file("extdata", package = "fffprocessr")
#' combine_fff(load_icp(path), load_uv(path, UV254_1))
combine_fff <- function(icp = NULL, uv = NULL, subtract_blank = TRUE, focus = 10) {

  combined <- dplyr::bind_rows(icp, uv) %>%
    # add 3 * sigma detection limit as a new column:
    dplyr::group_by(date, sample, .data$param) %>%
    dplyr::mutate(
      blank = dplyr::if_else(sample == "blank" & .data$time > focus, .data$conc, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date, .data$param) %>%
    dplyr::mutate(three_sigma = 3 * stats::sd(.data$blank, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$blank)

  out <- if(subtract_blank) {
    combined  %>%
      tidyr::pivot_wider(
        -file, names_from = sample,
        values_from = .data$conc, values_fn = mean
      ) %>%
      # linear interpolation of blank here
      # (for subtraction when times don't exactly match):
      dplyr::arrange(date, .data$param, .data$time) %>%
      dplyr::group_by(date, .data$param) %>%
      dplyr::mutate_at(
        dplyr::vars(tidyselect::matches("^blank$")),
        purrr::possibly(imputeTS::na_interpolation, otherwise = NA_real_)
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("sample_"),
        names_to = "sample",
        values_to = "conc",
        # next arg makes explicit NAs due to pivot_wider() / pivot_longer() combo implicit:
        values_drop_na = TRUE
      ) %>%
      # only subtract blank if it is not NA
      dplyr::mutate(
        conc = dplyr::if_else(is.na(.data$blank), .data$conc, .data$conc - .data$blank)
      )

  } else combined

  out %>%
    dplyr::select(date, sample, .data$param, .data$time, .data$conc, .data$three_sigma)
}
