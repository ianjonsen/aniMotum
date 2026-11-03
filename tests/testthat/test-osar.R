context("test osar")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)

xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))
res <- osar(xs)

test_that("r has s3 classes `osar`, `tbl_df`, `tbl`, `data.frame`", {
  skip_on_cran()
  expect_s3_class(res, c("osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  skip_on_cran()
  expect_silent(plot(res, "qq"))
})

test_that("plot returns a warning for deprecated plot type", {
  skip_on_cran()
  expect_warning(plot(res, type = "hist"))
})

test_that("plot completes silently",{
  skip_on_cran()
  expect_silent(plot(res, "ts"))
})

test_that("plot completes silently",{
  skip_on_cran()
  expect_silent(plot(res, "acf"))
})

test_that("plot returns a list w class gg, ggplot elements", {
  skip_on_cran()
  tp <- plot(res, "qq", pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})
## issue #91: when some individuals' residuals cannot be calculated, the
## remaining residuals must stay with their own ids and dates, and the
## warning must name the individual that failed
osar_partial <- function(model) {
  set.seed(11)
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  mk <- function(id, lon0, lat0) {
    n <- 25
    data.frame(id = id, date = t0 + (0:(n - 1)) * 3600 * 12, lc = "G",
               lon = lon0 + cumsum(rnorm(n, 0.05, 0.01)),
               lat = lat0 + cumsum(rnorm(n, 0.04, 0.01)))
  }
  d <- rbind(mk("A", 70, -49), mk("B", 75, -52), mk("C", 80, -55))
  fit <- suppressWarnings(fit_ssm(d, spdf = FALSE, model = model, time.step = 24,
                                  control = ssm_control(verbose = 0)))
  r0 <- suppressWarnings(osar(fit))
  ## make the OSA calculation fail for B with both methods
  fit$ssm[[2]]$tmb <- list(broken = TRUE)
  expect_warning(r1 <- osar(fit), "individuals: B")
  expect_setequal(unique(r1$id), c("A", "C"))
  for (i in c("A", "C")) {
    expect_equal(r1$residual[r1$id == i], r0$residual[r0$id == i])
    expect_equal(r1$date[r1$id == i], r0$date[r0$id == i])
  }
}

test_that("osar keeps residuals with their own individuals after a partial failure (rw)", {
  skip_on_cran()
  osar_partial("rw")
})

test_that("osar keeps residuals with their own individuals after a partial failure (crw)", {
  skip_on_cran()
  osar_partial("crw")
})
