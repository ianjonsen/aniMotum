context("test plot_mpm")

## expect plot is silent
data(xm)
data(xs)

## plot gamma time-series on a single page
tp <- plot(xm, pages = 1, asp = 0, ncol = 1)
test_that("plot completes silently - fitted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma time-series on separate pages for each individual
tp <- plot(xm, pages = 0)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma-coloured locations along track
tp <- plot(xm, xs)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma-coloured locations along track in 2 columns
tp <- plot(xm, xs, ncol = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
