context("test fit_mpm")

## fit_mpm output is a compound tibble with class `mpm`
##  tests expect that fit$mpm are 8-element lists (if optimiser does not crash)
##  that have S3 class aniMotum

## generate ssm obj as quickly as possible
test_that("fit_mpm returns mpm list w 8 elements", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72, 
                control = ssm_control(verbose = 0))
  dmp <- grab(xs, "predicted")
  fmp <- fit_mpm(dmp, model = "jmpm", control = mpm_control(verbose = 0))
  expect_s3_class(fmp, "mpm_df")
  expect_equal(length(fmp$mpm), 1)
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
  expect_equal(length(fmp$mpm), 1)
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
  expect_true(fn$converged)
  expect_true(fc$converged)
  expect_equal(fn$mpm[[1]]$fitted$g, fc$mpm[[1]]$fitted$g, tolerance = 1e-6)
  ## one dt reset at the start of each track
  expect_equal(which(is.na(fn$mpm[[1]]$data$dt)), c(1, 41))
})

test_that("fit_mpm keeps id and date when coordinates are named x, y", {
  skip_on_cran()
  set.seed(7)
  n <- 30
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  d <- data.frame(id = "a", date = t0 + (0:(n - 1)) * 3600 * 6,
                  x = cumsum(rnorm(n, 2, 0.5)), y = cumsum(rnorm(n, 1.5, 0.5)))
  fj <- fit_mpm(d, model = "jmpm", control = mpm_control(verbose = 0))
  expect_true(fj$converged)
  fm <- fit_mpm(d, model = "mpm", control = mpm_control(verbose = 0))
  expect_true(fm$converged)
  expect_true(all(c("id", "date") %in% names(grab(fm, "fitted"))))
})

test_that("grab warns, and does not error, on a crashed jmpm fit", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf = FALSE, model = "rw", time.step = 72,
                control = ssm_control(verbose = 0))
  fmp <- fit_mpm(xs, model = "jmpm", control = mpm_control(verbose = 0))
  fmp$mpm[[1]] <- structure("Error", class = "try-error")
  fmp$converged <- FALSE
  expect_warning(out <- grab(fmp, "fitted"), "crashed")
  expect_equal(nrow(out), 0)
})
