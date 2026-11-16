context("test Kinéis standard-error inflation")

test_that("ssm_control validates kineis$se.inflation", {
  expect_equal(ssm_control()$kineis$se.inflation, "default")
  expect_equal(ssm_control(kineis = list(se.inflation = 2))$kineis$se.inflation, 2)
  expect_error(ssm_control(kineis = list(se.inflation = -1)))
  expect_error(ssm_control(kineis = list(se.inflation = NA)))
  expect_error(ssm_control(kineis = list(se.inflation = Inf)))
  expect_error(ssm_control(kineis = list(se.inflation = c(2, 3))))
})

test_that("inflation factor depends on observation types and model", {
  f <- aniMotum:::kineis_se_inflation
  ctl <- ssm_control()
  expect_equal(f(rep("KI", 5), "crw", ctl)$factor, 1.8)
  expect_equal(f(rep("KI", 5), "rw", ctl)$factor, 1.15)
  expect_equal(f(rep("KI", 5), "mp", ctl)$factor, 1.75)
  expect_equal(f(rep("KI", 5), "crw", ctl)$ellipse95, 2.5)
  expect_equal(f(rep("KI", 5), "rw", ctl)$ellipse95, 1.65)
  expect_equal(f(rep("KI", 5), "mp", ctl)$ellipse95, 2.35)
  expect_equal(f(rep("KI", 5), "crw", ctl)$interval95, 2.4)
  expect_equal(f(rep("KI", 5), "rw", ctl)$interval95, 1.55)
  expect_equal(f(rep("KI", 5), "mp", ctl)$interval95, 2.25)
  expect_equal(f(c("KI", "GPS"), "crw", ctl)$ellipse95, 1)
  expect_equal(f(rep("KI", 5), "crw", ssm_control(kineis = list(se.inflation = 3)))$ellipse95, 3)
  expect_equal(f(c("KI", "GPS"), "crw", ctl)$factor, 1)
  expect_equal(f(c("KF", "LS"), "crw", ctl)$factor, 1)
  expect_equal(f(rep("KI", 5), "crw", ssm_control(kineis = list(se.inflation = 3)))$factor, 3)
  ## a control list without se.inflation gets the default
  ctl$kineis$se.inflation <- NULL
  expect_equal(f(rep("KI", 5), "crw", ctl)$factor, 1.8)
})

test_that("inflation multiplies standard errors only", {
  s <- data.frame(x = 1:2, y = 1:2, x.se = c(1, 2), y.se = c(2, 4), u.se = c(1, 1), g.se = c(0.1, 0.2))
  out <- aniMotum:::kineis_apply_se_inflation(s, 2)
  expect_equal(out$x, s$x)
  expect_equal(out$x.se, c(2, 4))
  expect_equal(out$y.se, c(4, 8))
  expect_equal(out$u.se, c(2, 2))
  expect_equal(out$g.se, s$g.se)
  expect_identical(aniMotum:::kineis_apply_se_inflation(s, 1), s)
  expect_null(aniMotum:::kineis_apply_se_inflation(NULL, 2))
})

test_that("non-Kinéis fits are not inflated", {
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = 24,
               control = ssm_control(verbose = 0))
  expect_equal(f$ssm[[1]]$se.inflation$factor, 1)
})

test_that("kineis_inflation checks its input", {
  argos <- data.frame(id = "a", date = as.POSIXct("2026-01-01", tz = "UTC") + 0:1 * 3600,
                      lc = "A", lon = 130, lat = -12, error.radius = NA)
  gps <- argos[, c("id", "date", "lon", "lat")]
  expect_error(kineis_inflation(argos, gps), "no Kin")
  expect_error(kineis_inflation(argos[, -6], gps), "missing column")
  expect_error(kineis_inflation(argos, gps[, -4]), "missing column")
  expect_error(kineis_inflation(argos, gps, coverage = 1.2), "coverage")
  expect_error(kineis_inflation(argos, "no_such_file.csv"), "not found")
})

test_that("SMRU and aniMotum formats give the same locations", {
  diag <- data.frame(REF = c("b", "a", "a"),
                     D_DATE = c("05/21/26 07:13:21", "05/21/26 09:00:00", "05/21/26 08:00:00"),
                     LQ = c(3, -2, -9), LAT = c(-11.7, -11.8, -11.9), LON = 135,
                     ERROR_RADIUS = c(162, 457, NA))
  smru <- aniMotum:::kineis_inflation_locations(diag, "argos")
  expect_equal(smru$lc, c("3", "B", "Z"))
  expect_equal(smru$date[1], as.POSIXct("2026-05-21 07:13:21", tz = "UTC"))
  std <- aniMotum:::kineis_inflation_locations(
    data.frame(id = smru$id, date = format(smru$date, "%Y-%m-%dT%H:%M:%SZ"),
               lc = smru$lc, lon = smru$lon, lat = smru$lat,
               error.radius = smru$error.radius), "argos")
  expect_equal(std, smru)
  expect_error(aniMotum:::kineis_inflation_dates("21 May 2026", "argos"), "could not be read")
})

test_that("confidence region scales use the stored 95% factors", {
  fits <- list(id = c("a", "b", "c"),
               ssm = list(list(se.inflation = list(factor = 1.8, ellipse95 = 2.5, interval95 = 2.4)),
                          list(se.inflation = list(factor = 1, ellipse95 = 1, interval95 = 1)),
                          list(opt = NULL)))
  id <- c("a", "a", "b", "c", "d")
  expect_equal(aniMotum:::kineis_ci_scale(fits, id, "ellipse"), c(2.5 / 1.8, 2.5 / 1.8, 1, 1, 1))
  expect_equal(aniMotum:::kineis_ci_scale(fits, id, "interval"), c(2.4 / 1.8, 2.4 / 1.8, 1, 1, 1))
  ## fits made before the 95% factors were stored
  old <- list(id = "a", ssm = list(list(se.inflation = list(factor = 1.8, ci95 = 2.4))))
  expect_equal(aniMotum:::kineis_ci_scale(old, "a"), 1)
})

test_that("elps() draws the 95% bivariate normal ellipse", {
  e <- aniMotum:::elps(0, 0, 2, 1, 90)
  r <- sqrt(qchisq(0.95, 2))
  expect_equal(max(abs(e[, 1])), 2 * r, tolerance = 1e-3)
  expect_equal(max(abs(e[, 2])), 1 * r, tolerance = 1e-2)
  expect_equal(max(abs(aniMotum:::elps(0, 0, 2, 1, 90, conf = FALSE)[, 1])), 2, tolerance = 1e-3)
  ## proportion of bivariate normal draws inside the ellipse
  set.seed(1)
  z <- cbind(rnorm(1e5, sd = 2), rnorm(1e5, sd = 1))
  inside <- (z[, 1] / (2 * r))^2 + (z[, 2] / (1 * r))^2 <= 1
  expect_equal(mean(inside), 0.95, tolerance = 0.005)
})
