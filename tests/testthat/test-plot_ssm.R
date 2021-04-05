context("test plot_ssm")

## expect plot is silent
data(xs)

## plot fitted value ts
tp <- plot(xs, what = "fitted")
test_that("plot completes silently - fitted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value ts
tp <- plot(xs, what = "predicted", type = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value 2-D
tp <- plot(xs, what = "predicted", type = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value 2-D, iwthout outliers
tp <- plot(xs, what = "predicted", type = 2, outlier = FALSE)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
