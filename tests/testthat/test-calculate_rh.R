test_that("calculate_rh() yields the expected hydrodynamic radii", {
  tr <- seq(11.6, 15.6, by = 1)
  ref <- c(1.055725e-11, 3.375582e-10, 6.645592e-10, 9.915602e-10, 1.318561e-09)
  calculated <- calculate_rh(tr)
  expect_equal(calculated, ref)
})
