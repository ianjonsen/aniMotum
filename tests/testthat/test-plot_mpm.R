context("test plot_mpm")

## expect plot is silent
## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(verbose = 0))
xm <- fit_mpm(xs, model = "jmpm")

## plot gamma time-series on a single page
test_that("plot completes silently - fitted", {
  tp <- plot(xm, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma time-series on separate pages for each individual
test_that("plot completes silently - predicted", {
  tp <- plot(xm, pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})

## plot gamma-coloured locations along track
test_that("plot completes silently - predicted", {
  tp <- plot(xm, xs, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma-coloured locations along track & scale size according to se
test_that("plot completes silently - predicted", {
  tp <- plot(xm, xs, se = TRUE, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma-coloured locations along track in 2 columns
test_that("plot completes silently - predicted", {
  tp <- plot(xm, xs, pages = 1, ncol = 2)
  expect_s3_class(tp, c("gg","ggplot"))
})

test_that("plot completes silently - predicted", {
  tp <- plot(xm, xs, pages = 0, ask = TRUE)
  expect_type(tp, "logical")
})

test_that("plot completes silently - predicted", {
  tp <- plot(xm, xs, pal = "Cividis", rev = TRUE, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
