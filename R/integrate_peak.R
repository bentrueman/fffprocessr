
#' Integrate FFF peaks
#'
#' @param x A numeric vector of retention times, in minutes.
#' @param y A numeric vector of detector responses, matching x in length.
#' @param injvol The injection volume in litres.
#' @param flowrate The detector flow rate in litres per minute.
#'
#' @return A number representing the integrated peak area.
#' @export
#'
#' @examples
#' x <- seq(0, 10, by = .1)
#' y <- exp(-(x - 5) ^ 2 / 2) # gaussian peak
#' integrate_peak(x, y)
integrate_peak <- function(x, y, injvol = 0.001, flowrate = 0.001) {

  vol_per_pt <- mean(diff(x), na.rm = TRUE) * flowrate # in L

  sum(y, na.rm = TRUE) * vol_per_pt / injvol # in µg/L for FFF-ICP-MS peaks

}
