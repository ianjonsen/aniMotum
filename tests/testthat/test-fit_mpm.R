context("test fit_mpm")

## fit_mpm output is a compound tibble with class `mpm`
##  tests expect that fit$mpm are 8-element lists (if optimiser does not crash)
##  that have S3 class foieGras

## generate ssm obj as quickly as possible
test_that("fit_mpm returns mpm list w 8 elements", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72, 
                control = ssm_control(verbose = 0))
  dmp <- grab(xs, "predicted")
  fmp <- fit_mpm(dmp, model = "jmpm", control = mpm_control(verbose = 0))
  expect_s3_class(fmp, "mpm_df")
  expect_equal(length(fmp$mpm), 1)
})


test_that("fit_mpm fits directly to an ssm object", {
  skip_on_cran()
  xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72, 
                control = ssm_control(verbose = 0))
  fmp <- fit_mpm(xs, model = "jmpm", control = 
                   mpm_control(verbose = 0, 
                               optim = "optim", 
                               method = "L-BFGS-B",
                               lower = list(l_sigma_g = -1)
                               )
                 )
  expect_s3_class(fmp, "mpm_df")
  expect_equal(length(fmp$mpm), 1)
})