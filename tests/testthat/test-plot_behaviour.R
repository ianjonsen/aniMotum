context("test plot methods for behavioural models")

test_that("plot methods return ggplot objects for each type", {
  skip_on_cran()
  data(ellie)
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(1)
  m <- fit_bm(f, model = "mrm", reps = 3, control = mpm_control(verbose = 0))
  p <- plot(m, ask = FALSE)
  expect_true(is.list(p) && all(vapply(p, ggplot2::is_ggplot, logical(1))))
  expect_true(ggplot2::is_ggplot(plot(m, type = "track", pages = 1, land = FALSE)))
  expect_s3_class(plot(m, pages = 1), "patchwork")
  expect_error(plot(m, pal = "not a palette"), "hcl.palette")

  g <- fit_mpm(f, model = "mpm", reps = 3, control = mpm_control(verbose = 0))
  expect_true(ggplot2::is_ggplot(plot(g, ask = FALSE)[[1]]))
  expect_true(ggplot2::is_ggplot(plot(g, type = "track", pages = 1, land = FALSE)))

  skip_if_not_installed("hmmTMB")
  h <- fit_bm(f, model = "hmm", reps = 3, control = mpm_control(verbose = 0))
  expect_s3_class(plot(h), "patchwork")
  expect_true(ggplot2::is_ggplot(plot(h, type = "track", pages = 1, land = FALSE)))
})
