
# generate internal data for functions:

library("tidyverse")

mals_calib <- tibble::tribble(
  ~number, ~theta, ~coefficient,
  1,      7,  0.00178775,
  2,     12, 0.00368763,
  3,     20,  0.00271051,
  4,     28,  0.00176462,
  5,     36,  0.00231833,
  6,     44,  0.00152087,
  7,     52,  0.00185855,
  8,     60,  0.00194345,
  9,     68,  0.00194282,
  10,    76,  0.00206297,
  11,    84,  0.00215706,
  12,    90,  0.00248170,
  13,   100,  0.00213770,
  14,   108,  0.00192152,
  15,   116,  0.00199917,
  16,   124,  0.00181977,
  17,   132,  0.00161909,
  18,   140,  0.00135980,
  19,   148,  0.00175342,
  20,   156,  0.00158798,
  21,   164,  0.00202189
)

chamber_dims <- list(
  L = 27.75, # channel length (cm) (measured)
  # next three params defined in https://doi.org/10.1016/j.chroma.2018.04.056
  b1 = 2, # cm (measured)
  b2 = .5, # cm (measured)
  z1 = 3.4 # cm (measured)
)

usethis::use_data(mals_calib, chamber_dims, internal = TRUE, overwrite = TRUE)
