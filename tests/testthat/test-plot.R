context("test plot")

## expect plot is silent
data(fssm)

## plot fitted value ts
tp <- plot(fssm, what = "fitted")
test_that("plot completes silently - fitted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value ts
tp <- plot(fssm, what = "predicted", type = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot predicted value 2-D
tp <- plot(fssm, what = "predicted", type = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
