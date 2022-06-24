
#' Calculate FFF channel thickness
#'
#' @description Estimate the FFF channel thickness using the method described in Litzen (1993).
#'
#' @param t1 Retention time, in minutes.
#' @param D Diffusion coefficient of standard.
#' @param focus Focus period, in minutes.
#' @param transition Transition time between focusing and elution, in minutes.
#' @param w Nominal channel thickness, in metres.
#' @param Vc Cross-flow rate, in L/min.
#' @param Vout Detector flow rate, in L/min.
#' @param Vin Injection flow rate, in L/min.
#' @param temp Temperature, degrees Celsius.
#' @param eta Dynamic viscosity, in N * s / m^2.
#' @param dims A list with the elements `L`, `b1`, `b2`, and `z1`,
#' as defined in Wang et al. (2018, units of cm).
#' @param tol Solution is arrived at iteratively; `tol` represents the difference between successive iterations.
#' @param maxiter Maximum iterations.
#'
#' @return A numeric vector of length one representing the estimated FFF channel thickness.
#' @export
#'
#' @seealso \code{\link{calculate_rh}}
#'
#' @references
#'
#' \enumerate{
#' \item Wang, J.-L.; Alasonati, E.; Fisicaro, P.; Benedetti, M. F.; Martin, M. Theoretical and Experimental Investigation of the Focusing Position in Asymmetrical Flow Field-Flow Fractionation (AF4). Journal of Chromatography A 2018, 1561, 67–75. https://doi.org/10.1016/j.chroma.2018.04.056.
#' \item Litzen, Anne. Separation Speed, Retention, and Dispersion in Asymmetrical Flow Field-Flow Fractionation as Functions of Channel Dimensions and Flow Rates. Anal. Chem. 1993, 65 (4), 461–470. https://doi.org/10.1021/ac00052a025.
#' }
#'
#'
#' @examples
#' calculate_w(t1 = 21.2, D = .35e-10, Vc = .0015, Vin = 2e-4)
calculate_w <- function(
  t1, # retention time (min)
  D, # diffusion coefficient of standard
  focus = 10, # focusing period (min)
  transition = 1, # period after focus (min)
  w = 500 / 1e6, # nominal channel thickness (m)
  Vc = 0.0025, # cross flow (L/min)
  Vout = 0.001, # detector flow (L/min)
  Vin = 0.0005, # injection flow (L/min)
  temp = 25, # temperature (degrees C)
  # predict viscosity using https://doi.org/10.1002/cjce.5450710617
  eta = 8.9e-4, # dynamic viscosity of pure water at 25 deg. C (N * s / m^2 at 26 deg C)
  dims = fffprocessr:::chamber_dims,
  tol = 1e-6,
  maxiter = 10
)
{
  L <- dims$L # channel length (cm) (measured)
  # next six params defined in https://doi.org/10.1016/j.chroma.2018.04.056
  b1 <- dims$b1 # cm (measured)
  b2 <- dims$b2 # cm (measured)
  z1 <- dims$z1 # cm (measured)
  z2 <- L - 1
  S <- (b1 - b2)/(z2 - z1)
  Atot <- 0.5 * (b1 * z2 + b2 * (L - z1))
  zfoc <- z1 + (b1 - sqrt(b1^2 - 2 * S * (Vin/Vc) * Atot +
                            b1 * z1 * S))/S
  Leff <- L - zfoc
  Aeff <- Atot * (1 - Vin/Vc)
  Kb <- 1.38064852e-23
  temp <- temp + 273.15
  y <- 0.5 * b1 * z1
  alpha <- 1 - (b1 * zfoc - ((b1 - b2) * zfoc^2/(2 * Leff)) -
                  y)/Aeff
  factor <- log(1 + alpha * Vc/Vout)

  t0 <- 0.1 * Aeff * w * factor/Vc
  tr <- 60 * (t1 - t0 - focus - transition)
  w_new <- sqrt(D * 6 * tr / factor)

  iter <- 1 # iterations

  while(abs(w - w_new) >  tol & iter <= maxiter) {
    iter <- iter + 1
    w <- w_new
    t0 <- 0.1 * Aeff * w * factor/Vc
    tr <- 60 * (t1 - t0 - focus - transition)
    w_new <- sqrt(D * 6 * tr / factor)
  }

  w_new

}
