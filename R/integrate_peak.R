
#' Integrate FFF peaks
#'
#' @param x numeric
#' @param y numeric
#' @param injvol numeric
#' @param flowrate numeric
#'
#' @return
#' @export
#'
#' @examples
integrate_peak <- function(x, y, injvol = 0.001, flowrate = 0.001) {

  vol_per_pt <- mean(diff(x), na.rm = TRUE) * flowrate # in L

  sum(y, na.rm = TRUE) * vol_per_pt / injvol # in µg/L for FFF-ICP-MS peaks

}
