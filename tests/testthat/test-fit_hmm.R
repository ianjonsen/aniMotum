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
  ## psi is fixed: estimating it for ellie without prediction times gives a
  ##  non-positive-definite Hessian and a warning unrelated to fit_hmm()
  f <- fit_ssm(ellie, model = "crw", time.step = NA, map = list(psi = factor(NA)),
               control = ssm_control(verbose = 0))
  expect_error(fit_bm(f, model = "hmm"), "regular predicted locations")
  expect_error(fit_bm(1, model = "hmm"), "ssm_df fit object or a data frame")
  expect_error(fit_bm(f, model = "hmm", n_states = 1), "n_states")
})

test_that("fit_hmm returns pooled states and parameters", {
  skip_on_cran()
  skip_if_not_installed("hmmTMB")
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(1)
  h <- fit_bm(f, model = "hmm", reps = 3)
  expect_s3_class(h, "hmm_mi")
  expect_equal(nrow(h$states), nrow(grab(f, "predicted")))
  expect_equal(rowSums(as.matrix(h$states[, c("p1", "p2")])), rep(1, nrow(h$states)), tolerance = 1e-6)
  expect_true(all(h$par$step_mean_1 <= h$par$step_mean_2))
  expect_equal(nrow(h$par), 3 - h$n_failed)
  ## Rubin pooling on the working scale
  expect_true(h$n_pooled >= 1)
  ps <- h$par_summary
  expect_true(all(c("step_mean_1", "step_mean_2", "tpm_12", "tpm_21") %in% ps$parameter))
  expect_true(all(ps$lower <= ps$estimate & ps$estimate <= ps$upper, na.rm = TRUE))
  tpm <- ps$estimate[grepl("^tpm", ps$parameter)]
  expect_equal(tpm[1] + tpm[2], 1, tolerance = 1e-8)     # tpm_11 + tpm_12
  expect_equal(nrow(h$par_working), 2 * 4 + 2)           # 4 observation parameters x 2 states, 2 transitions
})

test_that("hmm_pool relabels and pools working-scale coefficients", {
  mk <- function(e) list(working = data.frame(name = c("S1>S2", "S2>S1", "step.mean.state1", "step.mean.state2"),
                                              estimate = e, se = rep(0.1, 4)),
                         invlink = list(step = list(mean = exp)))
  fits <- list(mk(c(-3, -2, log(2), log(10))), mk(c(-3.2, -2.2, log(2.2), log(11))))
  p <- aniMotum:::hmm_pool(fits, 2)
  expect_equal(p$n_pooled, 2)
  sm <- p$natural[p$natural$parameter == "step_mean_1", ]
  expect_equal(sm$estimate, exp(mean(log(c(2, 2.2)))))
  t12 <- p$natural$estimate[p$natural$parameter == "tpm_12"]
  expect_equal(t12, plogis(-3.1))
})
