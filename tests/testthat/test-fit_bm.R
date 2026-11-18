context("test fit_bm")

test_that("fit_bm dispatches to each behavioural model", {
  skip_on_cran()
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(1)
  g <- fit_bm(f, model = "mpm", reps = 3, control = mpm_control(verbose = 0))
  expect_s3_class(g, c("mpm_df", "behaviour_fit"))
  expect_equal(g$model, "jmpm")
  expect_equal(g$reps, 3)
  ## joint = FALSE fits each track separately
  g1 <- fit_bm(f, model = "mpm", reps = NULL, joint = FALSE,
                      control = mpm_control(verbose = 0))
  expect_equal(g1$model, "mpm")
  expect_equal(g1$reps, 0L)
  r <- fit_bm(f, model = "mrm", reps = 3, control = mpm_control(verbose = 0))
  expect_s3_class(r, c("mrm_mi", "behaviour_fit"))
  skip_if_not_installed("hmmTMB")
  h <- fit_bm(f, model = "hmm", reps = 3, n_states = 2, control = mpm_control(verbose = 0))
  expect_s3_class(h, c("hmm_mi", "behaviour_fit"))
})

test_that("fit_bm checks the model and its arguments", {
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  expect_error(fit_bm(f, model = "abc"), "should be one of")
  ## reps = NULL only for models that can be fitted without imputation
  expect_error(fit_bm(f, model = "mrm", reps = NULL), "multiple imputation")
  ## arguments of another model are refused, naming the model's own
  expect_error(fit_bm(f, model = "mrm", n_states = 3), "not used by model = \"mrm\"")
  expect_error(fit_bm(f, model = "mpm", n_states = 3), "joint")
  ## every model's fitting function takes the shared arguments
  for (m in aniMotum:::behaviour_models())
    expect_true(all(aniMotum:::behaviour_shared_arguments() %in% names(formals(m$fit))))
})
