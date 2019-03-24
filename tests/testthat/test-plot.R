context("test plot")

## expect plot is silent
data(ellie)
dkf <- ellie[seq(1, nrow(ellie), by = 5),]
f <- fit_ssm(dkf, model = "crw", time.step = 24)

## plot fitted value ts
test_that("plot completes silently - fitted", {
  expect_silent(plot(f$ssm[[1]], what = "fitted"))
})

## plot predicted value ts
test_that("plot completes silently - predicted", {
  expect_silent(plot(f$ssm[[1]], what = "predicted"))
})

## plot predicted value ts w outliers
test_that("plot completes silently - predicted w outlier", {
  expect_silent(plot(f$ssm[[1]], what = "predicted", outlier = TRUE))
})

system("rm Rplots.pdf")
