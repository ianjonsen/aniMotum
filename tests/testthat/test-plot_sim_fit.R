context("test plot_sim_fit")
skip_on_cran()
## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, spdf=FALSE, model = "crw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))
trs <- sim_fit(xs, reps = 2, what = "fitted")

## expect plot is silent
test_that("plot completes silently", {
  tp <- plot(trs, ncol = 1)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

## expect plot is silent
test_that("plot completes silently", {
  tp <- plot(trs, zoom=TRUE)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

## expect plot is silent
test_that("plot completes silently", {
  tp <- plot(trs)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})