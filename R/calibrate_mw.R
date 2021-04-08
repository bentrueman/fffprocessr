
#' Perform a molecular weight calibration
#'
#' @param time A numeric vector of retention times.
#' @param mw A numeric vector of molecular weights.
#' @param type Chosse from a "linear" or "quadratic" fit.
#' @param newdata A vector of retention times or molecular weights.
#' @param output Choose the output type: "time" for retention time or "mw" for molecular weight.
#' Output "time" is an inverse prediction useful primarily for plotting.
#' @param predict Logical. Predict from the model?
#'
#' @return An object of class "lm" if predict = FALSE, or a vector of model predictions/inverse
#' predictions if predict = TRUE.
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' retention <- c(11.35890, 19.49773, 19.24173, 13.88310, 18.73001, 17.67315)
#' mw_kda <- c(0.69, 2000.00, 440.00, 1.80, 158.00, 44.00)
#' # return tick positions for 1, 10, 100, and 1000 kDa,
#' # based on a quadratic calibration curve:
#' calibrate_mw(retention, mw_kda, newdata = c(1, 10, 100, 1000), predict = TRUE)
calibrate_mw <- function(
  time,
  mw,
  type = "quadratic",
  newdata = NULL,
  output = "time",
  predict = TRUE
) {

  if(predict & is.null(newdata)) stop("Argument 'newdata' is missing with no default.")

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
                             time = c(
                               stats::coef(model)[1] - log10(mw),
                               stats::coef(model)[2],
                               stats::coef(model)[3]
                             ) %>%
                               polyroot() %>%
                               as.numeric() %>%
                               max()
                           ) %>%
                           dplyr::pull(time))
      } else
        if(output == "time" & type == "linear") {
          tibble::tibble(mw = newdata) %>%
            dplyr::mutate(time = (log10(mw) - stats::coef(model)[1]) / stats::coef(model)[2]
            ) %>%
            dplyr::pull(time)
        } else "Select a valid curve (quadratic or linear) and output (time or mw) type."
  } else model

}
