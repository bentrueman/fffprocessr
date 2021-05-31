
#' Calculate hydrodynamic radii
#'
#' @param t1 Retention time, in minutes.
#' @param focus Focus period, in minutes.
#' @param transition Transition time between focusing and elution, in minutes.
#' @param w Channel thickness, in metres.
#' @param Vc Cross-flow rate, in L/min.
#' @param Vout Detector flow rate, in L/min.
#' @param Vin Injection flow rate, in L/min.
#' @param temp Temperature, degrees Celsius.
#' @param eta Dynamic viscosity, in N * s / m^2.
#'
#' @return A vector of hydrodynamic radii in metres.
#' @export
#'
#' @examples
#' tr <- seq(11.6, 15.6, by = .1)
#' calculate_rh(tr)
calculate_rh <- function(
  t1, # retention time (min)
  focus = 10, # focusing period (min)
  transition = 1, # period after focus (min)
  w = 500 / 1e6, # channel thickness (m)
  Vc = 0.0025, # cross flow (L/min)
  Vout = 0.001, # detector flow (L/min)
  Vin = 0.0005, # injection flow (L/min)
  temp = 25, # temperature (degrees C)
  # predict viscosity using https://doi.org/10.1002/cjce.5450710617
  eta = 8.9e-4 # dynamic viscosity of pure water at 25 deg. C (N * s / m^2 at 26 deg C)
) {

  # calculate zfoc (https://doi.org/10.1016/j.chroma.2018.04.056)

  ## AF42000 channel properties (some depend on Vin / Vc):
  L <- 27.75 # channel length (cm) (measured)
  # next six params defined in https://doi.org/10.1016/j.chroma.2018.04.056
  b1 <- 2 # cm (measured)
  b2 <- .5 # cm (measured)
  z1 <- 3.4 # cm (measured)
  z2 <- L - 1 # cm (measured)
  S <- (b1 - b2) / (z2 - z1)
  Atot <- .5 * (b1 * z2 + b2 * (L - z1)) # total channel area (cm ^ 2)

  zfoc <- z1 + (b1 - sqrt(b1 ^ 2 - 2 * S * (Vin / Vc) * Atot + b1 * z1 * S)) / S

  # notes:

  ## see doi:10.1016/j.progpolymsci.2008.11.001
  ## doi:10.1021/ac00136a016
  ## doi: 10.1038/nprot.2015.009
  #
  ## eqn 1: tr <- (w ^ 2 / (6 * Dt)) * log(1 + Vc / Vout)
  ## eqn 2: Dt <- Kb * T / 6 * pi * eta * rh (Stokes-Einstein eqn)
  ## sub 2 into 1, solve for rh

  # constants and derived variables

  Leff <- L - zfoc # effective channel length (cm)
  Aeff <- Atot * (1 - Vin / Vc) # effective channel area
  Kb <- 1.38064852e-23 # Boltzmann constant (m2 kg s-2 K-1)
  temp <- temp + 273.15 # convert to absolute temp
  y <- .5 * b1 * z1 # inlet triangle area (cm^2)

  alpha <- 1 - (b1 * zfoc - ((b1 - b2) * zfoc ^ 2 / (2 * Leff)) - y) / Aeff # unitless

  factor <- log(1 + alpha * Vc / Vout) # unitless

  # based on doi:10.1007/978-3-7091-0154-4
  t0 <- 0.1 * Aeff * w * factor / Vc # in minutes

  tr <- 60 * (t1 - t0 - focus - transition) # convert to seconds

  Kb * temp * tr / (pi * eta * w ^ 2 * factor) # return rh, hydrodynamic radius

}
