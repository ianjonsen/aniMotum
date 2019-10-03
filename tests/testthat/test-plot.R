context("test plot")

## expect plot is silent
data(ssm_fits)

## plot fitted value ts
tp <- plot(ssm_fits, what = "fitted")
test_that("plot completes silently - fitted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value ts
tp <- plot(ssm_fits, what = "predicted", type = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value 2-D
tp <- plot(ssm_fits, what = "predicted", type = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value ts w outliers
tp <- plot(ssm_fits, what = "predicted", outlier = TRUE)
test_that("plot completes silently - predicted w outlier", {
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
