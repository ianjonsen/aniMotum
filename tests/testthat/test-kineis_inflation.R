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
  expect_equal(f(rep("KI", 5), "crw", ctl)$factor, 2.5)
  expect_equal(f(rep("KI", 5), "rw", ctl)$factor, 1.65)
  expect_equal(f(rep("KI", 5), "mp", ctl)$factor, 1)
  expect_equal(f(c("KI", "GPS"), "crw", ctl)$factor, 1)
  expect_equal(f(c("KF", "LS"), "crw", ctl)$factor, 1)
  expect_equal(f(rep("KI", 5), "crw", ssm_control(kineis = list(se.inflation = 3)))$factor, 3)
  ## a control list without se.inflation gets the default
  ctl$kineis$se.inflation <- NULL
  expect_equal(f(rep("KI", 5), "crw", ctl)$factor, 2.5)
})

test_that("inflation multiplies standard errors only", {
  s <- data.frame(x = 1:2, y = 1:2, x.se = c(1, 2), y.se = c(2, 4), u.se = c(1, 1))
  out <- aniMotum:::kineis_apply_se_inflation(s, 2)
  expect_equal(out$x, s$x)
  expect_equal(out$x.se, c(2, 4))
  expect_equal(out$y.se, c(4, 8))
  expect_equal(out$u.se, c(2, 2))
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
  d <- data.frame(id = "a", date = as.POSIXct("2026-01-01", tz = "UTC") + 0:1 * 3600,
                  lc = "G", lon = 130, lat = -12, error.radius = NA)
  expect_error(kineis_inflation(d), "no Kin")
  expect_error(kineis_inflation(d[, -6]), "missing column")
  expect_error(kineis_inflation(d, coverage = 1.2), "coverage")
})
