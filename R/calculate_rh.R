
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
#' @param dims A list with the elements `L`, `b1`, `b2`, and `z1`,
#' as defined in Wang et al. (2018, units of cm).
#'
#' @return A vector of hydrodynamic radii in metres.
#' @export
#'
#' @references
#'
#' \enumerate{
#' \item Wang, J.-L.; Alasonati, E.; Fisicaro, P.; Benedetti, M. F.; Martin, M. Theoretical and Experimental Investigation of the Focusing Position in Asymmetrical Flow Field-Flow Fractionation (AF4). Journal of Chromatography A 2018, 1561, 67–75. https://doi.org/10.1016/j.chroma.2018.04.056.
#' \item Kumar, A. A Simple Correlation for Estimating Viscosities of Solutions of Salts in Aqueous, Non-Aqueous and Mixed Solvents Applicable to High Concentration, Temperature and Pressure. Can. J. Chem. Eng. 1993, 71 (6), 948–954. https://doi.org/10.1002/cjce.5450710617.
#' \item Messaud, F. A.; Sanderson, R. D.; Runyon, J. R.; Otte, T.; Pasch, H.; Williams, S. K. R. An Overview on Field-Flow Fractionation Techniques and Their Applications in the Separation and Characterization of Polymers. Progress in Polymer Science 2009, 34 (4), 351–368. https://doi.org/10.1016/j.progpolymsci.2008.11.001.
#' \item Wahlund, K. Gustav.; Giddings, J. Calvin. Properties of an Asymmetrical Flow Field-Flow Fractionation Channel Having One Permeable Wall. Anal. Chem. 1987, 59 (9), 1332–1339. https://doi.org/10.1021/ac00136a016.
#' \item Tanase, M.; Zolla, V.; Clement, C. C.; Borghi, F.; Urbanska, A. M.; Rodriguez-Navarro, J. A.; Roda, B.; Zattoni, A.; Reschiglian, P.; Cuervo, A. M.; Santambrogio, L. Hydrodynamic Size-Based Separation and Characterization of Protein Aggregates from Total Cell Lysates. Nat Protoc 2015, 10 (1), 134–148. https://doi.org/10.1038/nprot.2015.009.
#' \item Field-Flow Fractionation in Biopolymer Analysis; Williams, S. K. R., Caldwell, K. D., Eds.; Springer Vienna: Vienna, 2012. https://doi.org/10.1007/978-3-7091-0154-4.
#' }
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
  eta = 8.9e-4, # dynamic viscosity of pure water at 25 deg. C (N * s / m^2 at 26 deg C)
  dims = chamber_dims
) {

  # calculate zfoc (https://doi.org/10.1016/j.chroma.2018.04.056)

  ## AF42000 channel properties (some depend on Vin / Vc):
  L <- dims$L # channel length (cm) (measured)
  # next six params defined in https://doi.org/10.1016/j.chroma.2018.04.056
  b1 <- dims$b1 # cm (measured)
  b2 <- dims$b2 # cm (measured)
  z1 <- dims$z1 # cm (measured)
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
