
#' Perform a molecular weight calibration
#'
#' @param time A numeric vector of retention times.
#' @param mw A numeric vector of molecular weights.
#' @param type "linear" or "quadratic"
#' @param newdata A vector of retention times or molecular weights.
#' @param output "time" or "mw"
#' @param predict Logical. Predict from the model?
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
calibrate_mw <- function(time, mw, type = "quadratic", newdata = NULL, output = "time", predict = TRUE) {

  model <- if(type == "quadratic") {
    stats::lm(log10(mw) ~ time + I(time ^ 2))
  } else
    if(type == "linear") {
      stats::lm(log10(mw) ~ time)
    } else "Select a valid curve type (quadratic or linear)."

  if(predict) {
    if(output == "mw") {
      stats::predict(model, newdata = newdata)
    } else
      if(output == "time" & type == "quadratic") {
        suppressWarnings(tibble::tibble(mw = newdata) %>%
                           dplyr::group_by(mw) %>%
                           dplyr::mutate(
                             time = c(stats::coef(model)[1] - log10(mw), stats::coef(model)[2], stats::coef(model)[3]) %>%
                               polyroot() %>%
                               as.numeric() %>%
                               max()
                           ) %>%
                           dplyr::pull(time))
      } else
        if(output == "time" & type == "linear") {
          tibble::tibble(mw = newdata) %>%
            dplyr::mutate(time = (mw - stats::coef(model)[1]) / stats::coef(model)[2]
            ) %>%
            dplyr::pull(time)
        } else "Select a valid curve (quadratic or linear) and output (time or mw) type."
  } else model

}
