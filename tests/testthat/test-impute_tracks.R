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
  m <- fit_bm(f, model = "mrm", reps = 3, control = mpm_control(verbose = 0))
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

test_that("the Kinéis error radius is read under other spellings of its name", {
  d <- data.frame(id = "a", date = as.POSIXct("2026-01-01", tz = "UTC") + 0:9 * 3600,
                  lon = 130 + 0:9 * 0.01, lat = -10, ERROR_RADIUS = 500)
  imp <- impute_tracks(d, reps = 1, time.step = 2)
  expect_equal(attr(imp, "source"), "kineis")
  ## pre-Kinéis SMRU data: semi-axes differ from the radius; the radius is used
  d$SEMI_MAJOR_AXIS <- c(rep(1500, 5), rep(500, 5))
  d$SEMI_MINOR_AXIS <- c(rep(200, 5), rep(500, 5))
  expect_message(imp <- impute_tracks(d, reps = 1, time.step = 2), "5 location\\(s\\) have an error radius")
  expect_equal(sum(attr(imp, "data")$keep), 10)
})

## location data other than Kinéis: errors from standard errors (degrees or
##  km), 95% intervals, or none
sd_ground <- function(imp) {
  e <- imp[imp$rep == 0, ]
  r <- imp[imp$rep > 0, ]
  i <- match(r$date, e$date)
  c(east = sd((r$lon - e$lon[i]) * pi / 180 * 6371.0088 * cos(r$lat * pi / 180)),
    north = sd((r$lat - e$lat[i]) * pi / 180 * 6371.0088))
}
locs <- function() {
  set.seed(3)
  data.frame(id = "a", date = as.POSIXct("2026-01-01", tz = "UTC") + 0:47 * 3600,
             lon = 130 + cumsum(rnorm(48, 0.01, 0.01)), lat = -10 + cumsum(rnorm(48, 0, 0.01)),
             depth = 1:48)
}

test_that("standard errors in km, in degrees and as 95% intervals give the ground SDs", {
  set.seed(1)
  d <- locs(); d$x.se <- 2; d$y.se <- 1
  imp <- impute_tracks(d, reps = 300, time.step = 1)
  expect_equal(attr(imp, "source"), "data")
  expect_match(attr(imp, "errors"), "x.se")
  expect_equal(unname(sd_ground(imp)), c(2, 1), tolerance = 0.05)
  ## degrees: 1 km north is 1 / 111.2 degrees of latitude
  d <- locs(); d$lat.se <- 1 / (6371.0088 * pi / 180); d$lon.se <- d$lat.se / cos(10 * pi / 180)
  imp <- impute_tracks(d, reps = 300, time.step = 1)
  expect_equal(unname(sd_ground(imp)), c(1, 1), tolerance = 0.05)
  ## the same values read as km
  d <- locs(); d$lon.se <- 2; d$lat.se <- 1
  imp <- impute_tracks(d, reps = 300, time.step = 1, error.units = "km")
  expect_equal(unname(sd_ground(imp)), c(2, 1), tolerance = 0.05)
  ## 95% intervals, in degrees
  d <- locs(); h <- 1.96 / (6371.0088 * pi / 180)
  d$lat.lci <- d$lat - h; d$lat.uci <- d$lat + h
  d$lon.lci <- d$lon - h / cos(10 * pi / 180); d$lon.uci <- d$lon + h / cos(10 * pi / 180)
  imp <- impute_tracks(d, reps = 300, time.step = 1)
  expect_equal(unname(sd_ground(imp)), c(1, 1), tolerance = 0.05)
})

test_that("locations without errors are taken as known, and the input is kept", {
  d <- locs()
  d$lon[5] <- NA
  expect_message(imp <- impute_tracks(d, reps = 2, time.step = 2), "taken as known")
  expect_equal(imp$lon[imp$rep == 1], imp$lon[imp$rep == 0])
  dat <- attr(imp, "data")
  expect_true(all(c("depth", "keep") %in% names(dat)))
  expect_equal(nrow(dat), nrow(d))
  expect_false(dat$keep[5])
  expect_error(impute_tracks(d[, c("id", "date", "lon")], reps = 2, time.step = 2), "lat")
})

test_that("behavioural estimates are interpolated to the observation times", {
  est <- data.frame(id = "a", segment = c(1, 1, 2), date = as.POSIXct("2026-01-01", tz = "UTC") + c(0, 2, 10) * 3600,
                    g = c(0.2, 0.6, 0.9))
  dat <- data.frame(id = "a", date = as.POSIXct("2026-01-01", tz = "UTC") + c(0, 1, 2, 5, 10) * 3600)
  out <- aniMotum:::bm_data(dat, est, "g")
  ## exact, halfway, exact, in the gap between segments (NA), exact
  expect_equal(out$g, c(0.2, 0.4, 0.6, NA, 0.9))
})

test_that("fit_bm returns the input data with the estimates at observation times", {
  skip_on_cran()
  set.seed(1)
  d <- locs(); d$x.se <- d$y.se <- 0.5
  d <- rbind(d, transform(d, id = "b", lat = lat - 1))
  m <- fit_bm(d, model = "mrm", reps = 3, time.step = 6, control = mpm_control(verbose = 0))
  expect_true(all(c("depth", "keep", "speed", "speed_lower", "speed_upper") %in% names(m$data)))
  expect_equal(nrow(m$data), nrow(d))
  g <- fit_bm(d, model = "mpm", reps = 3, time.step = 6, control = mpm_control(verbose = 0))
  expect_true(all(c("depth", "g", "lower", "upper") %in% names(g$data)))
  expect_equal(g$source, "data")
  ## without reps, their errors are not used
  suppressWarnings(expect_message(fit_bm(d, model = "mpm", reps = NULL, control = mpm_control(verbose = 0)),
                                  "not used without `reps`"))
})
