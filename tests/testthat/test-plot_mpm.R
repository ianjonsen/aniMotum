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
  expect_type(tp, "list")
})

## type = "track": all tracks on one map (pages = 1), one map per track (pages = 0)
test_that("type = track draws one map with pages = 1", {
  tp <- plot(xm, xs, type = "track", pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
  tp <- plot(xm, xs, type = "track", pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_length(tp, length(unique(xs$id)))
})

## type = timeseries with y supplied ignores the locations
test_that("type = timeseries with y gives the gamma time series", {
  tp <- plot(xm, xs, type = "timeseries", pages = 0, ask = FALSE)
  expect_equal(tp[[1]]$labels$y, "move persistence (g)")
})

## type = track needs the ssm fit
test_that("type = track without y errors", {
  expect_error(plot(xm, type = "track"), "needs the ssm fit")
})

test_that("plot completes silently - predicted", {
  tp <- plot(xm, xs, pal = "Cividis", rev = TRUE, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
})

## normalised g and its interval lie in 0 to 1
test_that("normalise rescales g to 0 to 1 within each track", {
  tp <- plot(xm, normalise = TRUE, pages = 0, ask = FALSE)
  d <- tp[[1]]$data
  expect_equal(range(d$g), c(0, 1))
  expect_true(all(d$lower >= 0 & d$upper <= 1))
})

## earlier fit_mpm() layout: g and g.se instead of logit_g and logit_g.se
test_that("plot works for the earlier g, g.se layout", {
  xo <- xm
  xo$mpm[[1]]$fitted <- with(xo$mpm[[1]]$fitted,
                             data.frame(id = id, date = date, g = g, g.se = logit_g.se))
  tp <- plot(xo, pages = 0, ask = FALSE)
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})

system("rm Rplots.pdf")
