context("simulated Argos error ellipses")

test_that("ellp.par errors match the covariance the model uses", {
  skip_on_cran()

  ## The error ellipse covariance used by crw.hpp, mp.hpp and the joint
  ## likelihoods is a rotation of diag(M2, m2) by the orientation eor. If
  ## ellp.par generates errors from that same covariance, then standardising
  ## each error by its own ellipse must give iid standard normals.
  ##
  ## This previously failed badly. eor was returned in degrees but passed to
  ## sin() and cos(), which take radians, and the off-diagonal was half what it
  ## should be, so the y errors were about 228 times too large in sd.

  set.seed(11)
  n <- 20000
  lc <- factor(sample(c("3", "2", "1", "0", "A", "B"), n, replace = TRUE),
               levels = c("3", "2", "1", "0", "A", "B"), ordered = TRUE)

  e <- aniMotum:::ellp.par(lc)

  expect_false(anyNA(e))
  expect_true(all(e$smaj > 0))
  expect_true(all(e$smin > 0))

  ## build each observation's covariance exactly as crw.hpp does, in km
  sM <- e$smaj / 1000
  sm <- e$smin / 1000
  cc <- e$eor * pi / 180
  psi <- 1
  M2 <- (sM / sqrt(2)) ^ 2
  m2 <- (sm * psi / sqrt(2)) ^ 2
  s11 <- M2 * sin(cc) ^ 2 + m2 * cos(cc) ^ 2
  s22 <- M2 * cos(cc) ^ 2 + m2 * sin(cc) ^ 2
  s12 <- 0.5 * (sM ^ 2 - (sm * psi) ^ 2) * cos(cc) * sin(cc)

  ## the determinant of a rotation of diag(M2, m2) is exactly M2 * m2
  expect_equal(s11 * s22 - s12 ^ 2, M2 * m2, tolerance = 1e-8)

  ## standardise each error by its own ellipse
  l11 <- sqrt(s11)
  l21 <- s12 / l11
  l22 <- sqrt(pmax(1e-15, s22 - l21 ^ 2))
  z1 <- e$x.err / l11
  z2 <- (e$y.err - l21 * z1) / l22

  expect_equal(var(z1), 1, tolerance = 0.05)
  expect_equal(var(z2), 1, tolerance = 0.05)
  expect_equal(cor(z1, z2), 0, tolerance = 0.05)
})
