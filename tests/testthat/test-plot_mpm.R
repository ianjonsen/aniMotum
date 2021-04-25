context("test plot_mpm")

## expect plot is silent
data(xm)
## generate fG_ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))

## plot gamma time-series on a single page
tp <- plot(xm, pages = 1, ncol = 1)
test_that("plot completes silently - fitted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma time-series on separate pages for each individual
tp <- plot(xm, pages = 0, ask = FALSE)
test_that("plot completes silently - predicted", {
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})

## plot gamma-coloured locations along track
tp <- plot(xm, xs, pages = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma-coloured locations along track & scale size according to se
tp <- plot(xm, xs, se = TRUE, pages = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma-coloured locations along track in 2 columns
tp <- plot(xm, xs, pages = 1, ncol = 2)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

tp <- plot(xm, xs, pages = 0, ask = TRUE)
test_that("plot completes silently - predicted", {
  expect_type(tp, "logical")
})

tp <- plot(xm, xs, pal = "Cividis", rev = TRUE, pages = 1)
test_that("plot completes silently - predicted", {
  expect_s3_class(tp, c("gg","ggplot"))
})

system("rm Rplots.pdf")
