context("test join with tag-measured events")

test_that("join interpolates behavioural estimates to event times", {
  skip_on_cran()
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  p <- grab(f, "predicted", as_sf = FALSE)
  set.seed(1)
  g <- fit_bm(f, model = "mpm", reps = 3, control = mpm_control(verbose = 0))
  r <- fit_bm(f, model = "mrm", reps = 3, control = mpm_control(verbose = 0))
  expect_true(all(c("rep", "id", "segment", "date", "g", "g.se") %in% names(g$imputed)))
  expect_true(all(c("rep", "log_rate", "speed") %in% names(r$imputed)))
  ev <- data.frame(id = p$id[c(5, 6)], date = p$date[c(5, 5)] + c(0, 12 * 3600), x = 1:2)
  j <- join(g, ev)
  expect_equal(nrow(j), 2)
  expect_true(all(c("x", "g", "lower", "upper") %in% names(j)))
  ## at a grid time, the estimate itself
  gf <- g$fitted[g$fitted$id == p$id[5] & g$fitted$date == p$date[5], ]
  expect_equal(j$g[1], gf$g)
  ## halfway between grid times, the mean of the two
  g2 <- g$fitted[g$fitted$id == p$id[5] & g$fitted$date == p$date[6], ]
  expect_equal(j$g[2], (gf$g + g2$g) / 2)
  jr <- join(g, ev, by_rep = TRUE)
  expect_equal(nrow(jr), 2 * length(unique(g$imputed$rep)))
  expect_true("rep" %in% names(jr))
  expect_true(all(c("speed", "log_rate") %in% names(join(r, ev))))
  ## events outside the tracks get NA
  out <- data.frame(id = p$id[1], date = min(p$date) - 86400)
  expect_true(is.na(join(g, out)$g))
  ## the ssm-mpm join still works, also by the old argument names
  xm <- fit_mpm(f, what = "p", model = "mpm", control = mpm_control(verbose = 0))
  expect_equal(nrow(join(f, xm)), nrow(p))
  expect_equal(nrow(join(ssm = f, mpm = xm)), nrow(p))
  expect_error(join(g, data.frame(a = 1)), "id")
})

test_that("fit_bm hmm fits extra data streams", {
  skip_on_cran()
  skip_if_not_installed("hmmTMB")
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  p <- grab(f, "predicted", as_sf = FALSE)
  set.seed(2)
  ## a stream recorded within each time step, related to the step length
  st <- data.frame(id = p$id, date = p$date - 6 * 3600, z = rgamma(nrow(p), 5, 1))
  h <- fit_bm(f, model = "hmm", reps = 2, n_states = 2, streams = st,
              stream_dists = list(z = "gamma2"), control = mpm_control(verbose = 0))
  expect_true("z" %in% names(h$states))
  expect_true(any(grepl("^z_mean_", h$par_summary$parameter)))
  expect_true(all(c("rep", "p1", "p2", "state") %in% names(h$imputed)))
  j <- join(h, p[1:3, c("id", "date")])
  expect_true(all(c("p1", "p2", "state") %in% names(j)))
  expect_error(fit_bm(f, model = "hmm", reps = 2, streams = st), "stream_dists")
  expect_error(fit_bm(f, model = "hmm", reps = 2, streams = st,
                      stream_dists = list(z.1 = "gamma2")), "dots")
})
