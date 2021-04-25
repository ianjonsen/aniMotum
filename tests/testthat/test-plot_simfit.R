context("test plot_simfit")

## generate fG_ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))
trs <- simfit(xs, reps = 2, what = "fitted")

## expect plot is silent
test_that("plot completes silently", {
  tp <- expect_warning(plot(trs, ncol = 2, pal = "Cividis", rev = TRUE))
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

## expect plot is silent
test_that("plot completes silently", {
  tp <- plot(trs)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})