context("test fit_ssm with Ornstein-Uhlenbeck velocity models")

data(ellie)

test_that("ou fits ellie and reports beta", {
  f <- fit_ssm(ellie, model = "ou", time.step = 24,
               control = ssm_control(verbose = 0))
  expect_s3_class(f, "ssm_df")
  expect_equal(unname(f$pmodel), "ou")
  expect_true(f$converged)
  expect_true("beta" %in% rownames(f$ssm[[1]]$par))
  p <- grab(f, "predicted")
  expect_true(all(c("u", "v", "s") %in% names(p)))
  expect_false(any(c("beta", "beta.se") %in% names(p)))
})

test_that("grab returns ou states as an sf tibble", {
  skip_on_cran()
  f <- fit_ssm(ellie, model = "ou", time.step = 24,
               control = ssm_control(verbose = 0))
  for (w in c("fitted", "predicted")) {
    ps <- grab(f, w, as_sf = TRUE)
    expect_s3_class(ps, "sf")
    expect_true(all(c("u", "v", "s", "s.se") %in% names(ps)))
  }
})

test_that("sim_post works for ou fits", {
  skip_on_cran()
  f <- fit_ssm(ellie, model = "ou", time.step = 24,
               control = ssm_control(verbose = 0))
  ps <- sim_post(f, what = "predicted", reps = 2)
  expect_s3_class(ps, "sim_post")
  expect_equal(nrow(ps$psims[[1]]), 3 * nrow(grab(f, "predicted")))
})

test_that("sim_fit refuses ou fits", {
  f <- fit_ssm(ellie, model = "ou", time.step = 24,
               control = ssm_control(verbose = 0))
  expect_error(sim_fit(f, what = "predicted", reps = 1), "'ou'")
})
