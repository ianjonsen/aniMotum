context("test fit_mpm")

## fit_mpm output is a list with class `mpm_df`, holding the fitted values
##  with the track locations, the parameter estimates, convergence, and,
##  without reps, the TMB objects

test_that("fit_mpm fits a data frame of locations", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72,
                control = ssm_control(verbose = 0))
  dmp <- grab(xs, "predicted")
  fmp <- fit_mpm(dmp, model = "jmpm", control = mpm_control(verbose = 0))
  expect_s3_class(fmp, "mpm_df")
  expect_equal(fmp$source, "data")
  expect_equal(nrow(fmp$fitted), nrow(dmp))
  expect_equal(fmp$fitted$lon, dmp$lon)
  expect_length(fmp$tmb, 1)
  expect_equal(fmp$reps, 0L)
})

test_that("fit_mpm fits directly to an ssm object", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72,
                control = ssm_control(verbose = 0))
  fmp <- fit_mpm(xs, model = "jmpm", control =
                   mpm_control(verbose = 0,
                               optim = "optim",
                               method = "L-BFGS-B",
                               lower = list(l_sigma_g = -1)
                               )
                 )
  expect_s3_class(fmp, "mpm_df")
  expect_equal(fmp$source, "ssm")
  expect_equal(fmp$what, "predicted")
  expect_equal(fmp$time.step, 72)
  expect_true(all(fmp$fitted$lower <= fmp$fitted$g & fmp$fitted$g <= fmp$fitted$upper))
  expect_true(all(c("sigma_g", "sigma_x", "sigma_y") %in% fmp$par$parameter))
  expect_equal(fmp$convergence$failed, 0)
  ## the TMB object is kept for the joint precision matrix
  sdp <- TMB::sdreport(fmp$tmb$joint, getJointPrecision = TRUE)
  expect_false(is.null(sdp$jointPrecision))
})

test_that("fit_mpm with mpm fits each track", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72,
                control = ssm_control(verbose = 0))
  ## one sese2 track does not converge (false convergence) with the mpm; it
  ##  is reported as failed and left out of the estimates
  fmp <- suppressWarnings(fit_mpm(xs, model = "mpm", control = mpm_control(verbose = 0)))
  expect_equal(nrow(fmp$convergence), length(unique(xs$id)))
  expect_length(fmp$tmb, length(unique(xs$id)))
  expect_setequal(unique(fmp$par$unit), fmp$convergence$unit[fmp$convergence$failed == 0])
  expect_warning(fit_mpm(xs, model = "mpm", control = mpm_control(verbose = 0)), "failed")
})

## issue #90
mk_track <- function(n, lon0, lat0, t0, seed) {
  set.seed(seed)
  data.frame(date = t0 + (0:(n - 1)) * 3600 * 6,
             lon = lon0 + cumsum(rnorm(n, 0.02, 0.005)),
             lat = lat0 + cumsum(rnorm(n, 0.015, 0.005)))
}

test_that("jmpm does not depend on how individuals are labelled (numeric ids, unequal track lengths)", {
  skip_on_cran()
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  trA <- mk_track(40, 70, -49, t0, 1)
  trB <- mk_track(25, 71, -50, t0 + 40 * 3600 * 6 + 3600, 101)
  dn <- rbind(cbind(id = 2, trA), cbind(id = 10, trB))[, c("id", "date", "lon", "lat")]
  dc <- rbind(cbind(id = "a", trA), cbind(id = "b", trB))[, c("id", "date", "lon", "lat")]
  fn <- fit_mpm(dn, model = "jmpm", control = mpm_control(verbose = 0))
  fc <- fit_mpm(dc, model = "jmpm", control = mpm_control(verbose = 0))
  expect_equal(fn$convergence$failed, 0)
  expect_equal(fc$convergence$failed, 0)
  expect_equal(fn$fitted$g, fc$fitted$g, tolerance = 1e-6)
  ## one dt reset at the start of each track
  d <- data.frame(id = dn$id, date = dn$date, x = dn$lon, y = dn$lat)
  f <- aniMotum:::mpmf(d, model = "jmpm", control = mpm_control(verbose = 0))
  expect_equal(which(is.na(f$data$dt)), c(1, 41))
})

test_that("fit_mpm keeps id and date when coordinates are named x, y", {
  skip_on_cran()
  set.seed(7)
  n <- 30
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  d <- data.frame(id = "a", date = t0 + (0:(n - 1)) * 3600 * 6,
                  x = cumsum(rnorm(n, 2, 0.5)), y = cumsum(rnorm(n, 1.5, 0.5)))
  fj <- fit_mpm(d, model = "jmpm", control = mpm_control(verbose = 0))
  expect_equal(fj$convergence$failed, 0)
  fm <- fit_mpm(d, model = "mpm", control = mpm_control(verbose = 0))
  expect_equal(fm$convergence$failed, 0)
  expect_true(all(c("id", "date") %in% names(grab(fm, "fitted"))))
  ## projected coordinates: no longitude or latitude
  expect_true(all(is.na(fm$fitted$lon)))
})

test_that("Kinéis locations without reps give an informative error", {
  d <- data.frame(id = "a", date = as.POSIXct("2024-01-01", tz = "UTC") + 3600 * (0:9),
                  lon = 130 + 0.01 * (0:9), lat = -12 + 0.01 * (0:9), error.radius = 0.5)
  expect_error(fit_mpm(d, control = mpm_control(verbose = 0)), "multiple imputation")
})

test_that("grab warns, and does not error, on a crashed jmpm fit before 1.3-1", {
  skip_on_cran()
  fmp <- dplyr::tibble(mpm = list(structure("Error", class = "try-error")),
                       converged = FALSE, model = "jmpm")
  class(fmp) <- append("mpm_df", class(fmp))
  expect_warning(out <- grab(fmp, "fitted"), "crashed")
  expect_equal(nrow(out), 0)
})
