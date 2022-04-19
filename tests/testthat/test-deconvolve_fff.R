
set.seed(0)
x <- seq(0, 10, length.out = 200)
# two gaussian peaks:
h1 <- 1
h2 <- 2
mu1 <- 3
mu2 <- 7
s1 <- 1
s2 <- 2
component <- function(h, mu, s) {
  h * exp(-0.5 * ((x - mu) / s) ^ 2)
}
noise <- rnorm(100, 0, .1)
y <- component(h1, mu1, s1) + component(h2, mu2, s2) + noise
plot(x, y)

test_that("deconvolve_fff() fits the expected model", {
  mod <- deconvolve_fff(x, y, h = c(1, 1), mu = c(2, 7), s = c(1, 1), fn = "normal")
  d <- coef(mod$model) - c(h1, h2, mu1, mu2, s1, s2)
  expect_lt(max(abs(d)), .05)
})
