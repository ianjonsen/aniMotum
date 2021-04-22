context("test fit_mpm")

## fit_mpm output is a compound tibble with class `fG_mpm`
##  tests expect that fit$mpm are 8-element lists (if optimiser does not crash)
##  that have S3 class foieGras
data(xs)
dmp <- grab(xs, "predicted", as_sf = FALSE)
dmp <- dmp[, c("id", "date", "lon", "lat")]

test_that("fit_mpm returns fG_mpm list w 8 elements", {
  fmp <- fit_mpm(dmp, model = "jmpm", control = mpm_control(verbose = 0))
  expect_s3_class(fmp, "fG_mpm")
  expect_equal(length(fmp$mpm[[1]]), 8)
})

test_that("fit_mpm returns fG_mpm list w 8 elements", {
  fmp <- fit_mpm(dmp, model = "mpm", control = mpm_control(verbose = 0))
  expect_s3_class(fmp, "fG_mpm")
  expect_equal(length(fmp$mpm[[1]]), 8)
})

test_that("fit_mpm fits directly to an fG_ssm object", {
  fmp <- fit_mpm(xs, model = "jmpm", control = mpm_control(verbose = 0))
  expect_s3_class(fmp, "fG_mpm")
  expect_equal(length(fmp$mpm[[1]]), 8)
})

test_that("fit_mpm fits directly to an fG_ssm object", {
  fmp <- fit_mpm(xs, model = "jmpm", control = 
                   mpm_control(verbose = 0, 
                               optim = "optim", 
                               method = "L-BFGS-B",
                               lower = list(l_sigma_g = -1)
                               )
                 )
  expect_s3_class(fmp, "fG_mpm")
  expect_equal(length(fmp$mpm[[1]]), 8)
})