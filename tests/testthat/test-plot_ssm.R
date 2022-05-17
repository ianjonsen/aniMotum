context("test plot_ssm")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, 
              spdf=FALSE, 
              model = "rw", 
              time.step=72, 
              control = ssm_control(verbose = 0))

## plot fitted value ts
test_that("plot completes silently - fitted", {
  tp <- plot(xs, what = "fitted", pages = 0, outlier = FALSE, ask = FALSE)
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})

## plot predicted value ts
test_that("plot completes silently - predicted", {
  tp <- plot(xs, what = "predicted", type = 1, pages = 1)
  expect_s3_class(tp, c("patchwork", "gg","ggplot"))
})

## plot predicted value ts
test_that("plot completes silently - predicted", {
  tp <- plot(xs, what = "predicted", type = 1, pages = 1, ncol = 2)
  expect_s3_class(tp, c("patchwork","gg","ggplot"))
})

## plot predicted value 2-D, iwthout outliers
test_that("plot completes silently - predicted", {
  tp <- plot(xs, what = "predicted", type = 2, pages = 1)
  expect_s3_class(tp, c("patchwork","gg","ggplot"))
})

## plot predicted value 2-D, iwthout outliers
test_that("plot completes silently - predicted", {
  tp <- plot(xs, what = "predicted", type = 2, pages = 1, pal = "Cividis")
  expect_s3_class(tp, c("patchwork","gg","ggplot"))
})

system("rm Rplots.pdf")
