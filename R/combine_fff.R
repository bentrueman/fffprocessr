
#' Combine loaded UV/Vis and ICP-MS data
#'
#' @param icp A tibble created by `load_icp()`
#' @param uv A tibble created by `load_uv()`
#' @param subtract_blank Logical. Subtract a blank from the sample runs?
#' @param focus The focusing period, in minutes.
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
combine_fff <- function(icp, uv, subtract_blank = TRUE, focus = 10) {

  combined <- dplyr::bind_rows(icp, uv) %>%
    dplyr::mutate(
      # extract sample name:
      sample = stringr::str_replace(file, "(.+)(\\d{4}-\\d{2}-\\d{2}_)(.+)(\\.[:alpha:]+)", "\\3"),
      # rename samples with "blank" in the name:
      sample = dplyr::if_else(stringr::str_detect(sample, "blank"), "blank", sample),
      sample = dplyr::if_else(sample == "blank", sample, paste0("sample_", sample))
    ) %>%
    # add 3 * sigma detection limit as a new column:
    dplyr::group_by(date, sample, param) %>%
    dplyr::mutate(blank = dplyr::if_else(sample == "blank" & time > focus, conc, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date, param) %>%
    dplyr::mutate(three_sigma = 3 * stats::sd(blank, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-blank)

  out <- if(subtract_blank) {
    combined  %>%
      tidyr::pivot_wider(-file, names_from = sample, values_from = conc, values_fn = mean) %>%
      # linear interpolation here:
      dplyr::arrange(date, param, time) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("sample_"), matches("^blank$")), imputeTS::na_interpolation) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with("sample_"), names_to = "sample", values_to = "conc") %>%
      dplyr::mutate(conc = conc - blank)
  } else combined

  out %>%
    dplyr::select(date, sample, param, time, conc, three_sigma)
}
