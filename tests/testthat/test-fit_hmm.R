context("test fit_hmm")

test_that("hmm_moves computes step lengths and turning angles", {
  d <- data.frame(id = "a",
                  date = as.POSIXct("2026-01-01", tz = "UTC") + 0:3 * 3600,
                  lon = c(0, 1, 1, 0), lat = c(0, 0, 1, 1))
  m <- aniMotum:::hmm_moves(d)
  ## 1 degree of longitude at the equator, on a sphere of radius 6371.0088 km
  expect_equal(m$step[2], 6371.0088 * pi / 180, tolerance = 1e-6)
  expect_true(is.na(m$step[1]) && is.na(m$angle[2]))
  ## east then north is a left turn of 90 degrees; north then west another
  expect_equal(m$angle[3], pi / 2, tolerance = 1e-3)
  expect_equal(m$angle[4], pi / 2, tolerance = 1e-2)
})

test_that("fit_hmm checks its input", {
  skip_if_not_installed("hmmTMB")
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = NA, control = ssm_control(verbose = 0))
  expect_error(fit_hmm(f), "regular predicted locations")
  f <- fit_ssm(ellie, model = "mp", time.step = 24, control = ssm_control(verbose = 0))
  expect_error(fit_hmm(f), "`crw` or `rw`")
})

test_that("fit_hmm returns pooled states and parameters", {
  skip_on_cran()
  skip_if_not_installed("hmmTMB")
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(1)
  h <- fit_hmm(f, reps = 3)
  expect_s3_class(h, "hmm_mi")
  expect_equal(nrow(h$states), nrow(grab(f, "predicted")))
  expect_equal(rowSums(as.matrix(h$states[, c("p1", "p2")])), rep(1, nrow(h$states)), tolerance = 1e-6)
  expect_true(all(h$par$step_mean_1 <= h$par$step_mean_2))
  expect_equal(nrow(h$par), 3 - h$n_failed)
})
