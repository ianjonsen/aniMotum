context("test plot_mpm")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=72,
              control = ssm_control(verbose = 0))
xm <- fit_mpm(xs, model = "jmpm", control = mpm_control(verbose = 0))

## fit_mpm() output before aniMotum 1.3-1: a tibble of fits, with g and g.se
##  in the fitted values and no locations
old_mpm <- function(xm) {
  f <- xm$fitted
  fit <- list(fitted = data.frame(id = f$id, date = f$date, g = f$g, g.se = 0.5),
              par = NULL, model = "jmpm", data = NULL, tmb = NULL, opt = NULL,
              rep = NULL, aic = NULL)
  out <- dplyr::tibble(mpm = list(fit), converged = TRUE, model = "jmpm")
  class(out) <- append("mpm_df", class(out))
  out
}

## plot gamma time-series on a single page
test_that("time series on one page", {
  tp <- plot(xm, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
})

## plot gamma time-series on separate pages for each individual
test_that("time series, one plot per track", {
  tp <- plot(xm, pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
  expect_equal(tp[[1]]$labels$y, "move persistence (g)")
})

## the fit holds its locations: track maps need no ssm fit
test_that("type = track draws one map with pages = 1", {
  tp <- plot(xm, type = "track", pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
  tp <- plot(xm, type = "track", pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_length(tp, length(unique(xs$id)))
})

test_that("track map with point size by standard error, and palette options", {
  expect_s3_class(plot(xm, type = "track", se = TRUE, pages = 1), c("gg","ggplot"))
  expect_s3_class(plot(xm, type = "track", pal = "Cividis", rev = TRUE, pages = 1),
                  c("gg","ggplot"))
})

test_that("ask = TRUE returns the plots invisibly", {
  tp <- plot(xm, type = "track", pages = 0, ask = TRUE)
  expect_type(tp, "list")
})

## a supplied ssm fit is not needed, and is ignored with a warning; it still
##  selects the track map by default
test_that("y is ignored for current fit_mpm() output", {
  expect_warning(tp <- plot(xm, xs, pages = 1), "ignored")
  expect_s3_class(tp, c("gg","ggplot"))
})

## normalised g and its interval lie in 0 to 1
test_that("normalise rescales g to 0 to 1 within each track", {
  tp <- plot(xm, normalise = TRUE, pages = 0, ask = FALSE)
  d <- tp[[1]]$data
  expect_equal(range(d$g), c(0, 1))
  expect_true(all(d$lower >= 0 & d$upper <= 1))
})

## earlier fit_mpm() output: time series alone, tracks with the ssm fit
test_that("plot works for fit_mpm() output before 1.3-1", {
  xo <- old_mpm(xm)
  tp <- plot(xo, pages = 0, ask = FALSE)
  expect_s3_class(tp[[1]], c("gg","ggplot"))
  tp <- plot(xo, xs, pages = 1)
  expect_s3_class(tp, c("gg","ggplot"))
  expect_error(plot(xo, type = "track"), "needs the ssm fit")
})

## a fit to projected coordinates has no longitude and latitude to map
test_that("type = track errors without longitude and latitude", {
  d <- grab(xs, "predicted")[, c("id", "date", "x", "y")]
  xp <- fit_mpm(d, model = "jmpm", control = mpm_control(verbose = 0))
  expect_s3_class(plot(xp, pages = 1), c("gg","ggplot"))
  expect_error(plot(xp, type = "track"), "projected coordinates")
})

if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
