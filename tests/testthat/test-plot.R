context("test plot")

## expect plot is silent
data(ellie)
dkf <- ellie[seq(1, nrow(ellie), by = 5),]
f <- fit_ssm(dkf, model = "crw", time.step = 24, verbose = 0)

## plot fitted value ts
tp <- plot(f, what = "fitted")
test_that("plot completes silently - fitted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value ts
tp <- plot(f, what = "predicted", type = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value 2-D
tp <- plot(f, what = "predicted", type = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value ts w outliers
tp <- plot(f, what = "predicted", outlier = TRUE)
test_that("plot completes silently - predicted w outlier", {
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
