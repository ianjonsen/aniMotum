context("test plot_ssm")

## expect plot is silent
data(xs)

## plot fitted value ts
tp <- plot(xs, what = "fitted", pages = 0, outlier = FALSE)
test_that("plot completes silently - fitted", {
  expect_type(tp, "logical")
})

## plot fitted value ts
tp <- plot(xs, what = "fitted", pages = 0, outlier = FALSE, ask = FALSE)
test_that("plot completes silently - fitted", {
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})

## plot predicted value ts
tp <- plot(xs, what = "predicted", type = 1, pages = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("patchwork", "gg","ggplot"))
})

## plot predicted value ts
tp <- plot(xs, what = "predicted", type = 1, pages = 1, ncol = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("patchwork","gg","ggplot"))
})

## plot predicted value 2-D, iwthout outliers
tp <- plot(xs, what = "predicted", type = 2, pages = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("patchwork","gg","ggplot"))
})

## plot predicted value 2-D, iwthout outliers
tp <- plot(xs, what = "predicted", type = 2, pages = 1, pal = "Cividis")
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("patchwork","gg","ggplot"))
})

system("rm Rplots.pdf")
