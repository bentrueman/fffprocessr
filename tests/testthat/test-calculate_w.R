test_that("calculate_w() returns the expect channel thickness.", {
  expect_equal(calculate_w(t1 = 21.2, D = .35e-10, Vc = .0015, Vin = 2e-4), 0.00038166305)
})
