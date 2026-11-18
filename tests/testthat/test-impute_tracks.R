context("test impute_tracks and multiple-imputation pooling")

test_that("regular_grid drops grid times in long gaps and numbers segments", {
  tt <- c(0, 1, 2, 3, 20, 21, 22)          # hours; a 17 h gap
  g <- aniMotum:::regular_grid(tt, time.step = 1, max_gap = 2)
  expect_equal(g$t, c(0, 1, 2, 3, 20, 21, 22))
  expect_equal(g$segment, c(1, 1, 1, 1, 2, 2, 2))
  g <- aniMotum:::regular_grid(tt, time.step = 0.5, max_gap = 2)
  expect_false(any(g$t > 3 & g$t < 20))
})

test_that("pool_rubin follows Rubin's rules", {
  est <- matrix(c(1, 3), nrow = 1)
  se <- matrix(c(1, 1), nrow = 1)
  p <- aniMotum:::pool_rubin(est, se)
  ## mean 2; within 1; between var(c(1, 3)) = 2; total 1 + 1.5 * 2 = 4
  expect_equal(p$estimate, 2)
  expect_equal(p$se, 2)
  expect_equal(p$fraction_missing, 3 / 4)
  ## no between-imputation variance: total = within, infinite df
  p <- aniMotum:::pool_rubin(matrix(c(1, 1), nrow = 1), se)
  expect_equal(p$se, 1)
  expect_equal(p$df, Inf)
})

test_that("Kinéis imputation draws errors from the error radius", {
  set.seed(1)
  d <- data.frame(id = "a", date = as.POSIXct("2026-01-01", tz = "UTC") + 0:47 * 3600,
                  lon = 130 + 0:47 * 0.01, lat = -10, error.radius = 1000)
  ## grid times on the locations (w = 0): imputed - point estimate has SD
  ##  1.061 km east and 0.739 km north for a 1 km radius
  imp <- impute_tracks(d, reps = 400, time.step = 1)
  e <- imp[imp$rep == 0, ]
  expect_equal(e$lon, d$lon, tolerance = 1e-8)
  r <- imp[imp$rep > 0, ]
  i <- match(r$date, e$date)
  east <- (r$lon - e$lon[i]) * pi / 180 * 6371.0088 * cos(-10 * pi / 180)
  north <- (r$lat - e$lat[i]) * pi / 180 * 6371.0088
  expect_equal(sd(east), 1.061, tolerance = 0.03)
  expect_equal(sd(north), 0.739, tolerance = 0.03)
  expect_equal(attr(imp, "source"), "kineis")
  expect_error(impute_tracks(d, reps = 2), "time.step")
})

test_that("SSM imputation returns the point estimate as rep 0", {
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(1)
  imp <- impute_tracks(f, reps = 3)
  expect_equal(sort(unique(imp$rep)), 0:3)
  expect_equal(imp$lon[imp$rep == 0], grab(f, "predicted", as_sf = FALSE)$lon)
  expect_equal(attr(imp, "source"), "ssm")
})

test_that("fit_mrm pools the movement rate across imputations", {
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(1)
  m <- fit_bm(f, model = "mrm", reps = 3)
  expect_s3_class(m, "mrm_mi")
  expect_equal(nrow(m$fitted), nrow(grab(f, "predicted")))
  expect_true(all(m$fitted$lower <= m$fitted$rate & m$fitted$rate <= m$fitted$upper))
  expect_setequal(m$par$parameter, c("sigma_rate", "rho_p"))
  ## speed (km/h) at the time step: rate * sqrt(2 / time.step)
  expect_equal(m$fitted$speed, m$fitted$rate * sqrt(2 / 24))
  expect_equal(m$fitted$speed_upper, m$fitted$upper * sqrt(2 / 24))
})

test_that("fit_mpm with reps pools move persistence across imputations", {
  data(sese2)
  f <- fit_ssm(sese2, model = "rw", time.step = 72, control = ssm_control(verbose = 0))
  set.seed(1)
  m <- fit_mpm(f, model = "jmpm", reps = 3, control = mpm_control(verbose = 0))
  expect_s3_class(m, "mpm_df")
  expect_equal(m$reps, 3)
  expect_equal(m$source, "ssm")
  expect_null(m$tmb)
  expect_true(all(m$fitted$fraction_missing >= 0 & m$fitted$fraction_missing <= 1))
  expect_true(all(m$fitted$g >= 0 & m$fitted$g <= 1))
  expect_true(all(c("sigma_g", "sigma_x", "sigma_y") %in% m$par$parameter))
})

test_that("imputed tracks are projected in a crs suited to the data, or the one given", {
  set.seed(1)
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")
  ## high southern latitudes, wide latitude range: not Mercator
  d <- data.frame(id = "a", date = t0 + 0:99 * 3600 * 6,
                  lon = 70 + 0:99 * 0.1, lat = -48 - 0:99 * 0.2, error.radius = 500)
  imp <- impute_tracks(d, reps = 2, time.step = 12)
  expect_false(grepl("merc", attr(imp, "crs")))
  xy <- sf::sf_project("+proj=longlat +datum=WGS84", attr(imp, "crs"), cbind(imp$lon, imp$lat))
  expect_equal(unname(xy[, 1]), imp$x, tolerance = 1e-6)
  expect_equal(unname(xy[, 2]), imp$y, tolerance = 1e-6)
  ## a given crs is used
  prj <- "+proj=stere +lat_0=-90 +lon_0=70 +datum=WGS84 +units=km +no_defs"
  imp <- impute_tracks(d, reps = 2, time.step = 12, crs = prj)
  expect_equal(attr(imp, "crs"), prj)
  ## an SSM fit's simulated tracks are reprojected to the chosen crs
  f <- fit_ssm(sese2, model = "rw", time.step = 72, control = ssm_control(verbose = 0))
  imp <- impute_tracks(f, reps = 2)
  expect_equal(attr(imp, "crs"), auto_crs(imp$lon[imp$rep == 0], imp$lat[imp$rep == 0]))
  m <- fit_mpm(f, model = "jmpm", control = mpm_control(verbose = 0))
  expect_equal(m$crs, attr(imp, "crs"))
})

test_that("vmax drops extreme Kinéis locations before imputation", {
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")
  d <- data.frame(id = "a", date = t0 + 0:49 * 3600 * 2,
                  lon = 140 + 0:49 * 0.02, lat = -60 - 0:49 * 0.01, error.radius = 500)
  ## a spike: 2 degrees of latitude out and back in 2 h
  d$lat[25] <- d$lat[25] + 2
  expect_message(imp <- impute_tracks(d, reps = 1, time.step = 6, vmax = 4), "dropped 1 of 50")
  expect_true(max(imp$lat) < -59.5)
  ## without vmax the spike is interpolated into the track
  imp <- impute_tracks(d, reps = 1, time.step = 2)
  expect_true(max(imp$lat) > -59)
  expect_error(impute_tracks(d, reps = 1, time.step = 6, vmax = -1), "vmax")
})
