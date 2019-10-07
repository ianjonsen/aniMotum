context("test fit_mpm")

## fit_mpm output is a compound tibble with class `fG_mpm`
##  tests expect that fit$mpm are 7-element lists (if optimiser does not crash)
##  that have S3 class foieGras
data(fssm)
dmp <- fssm %>% 
  grab(., "predicted", as_sf = FALSE) %>%
  select(id,date,lon,lat)

## step through prefilter-specific arguments first
## minimum specified arguments - crw

test_that("fit_mpm returns fG_mpm list w 8 elements", {
  skip_on_cran()
  fmp <- fit_mpm(dmp, model = "jmpm")
  expect_s3_class(fmp, "fG_mpm")
  expect_equal(length(fmp$mpm[[1]]), 8)
})


test_that("fit_mpm returns fG_mpm list w 8 elements", {
  skip_on_cran()
  fmp <- fit_mpm(dmp, model = "mpm")
  expect_s3_class(fmp, "fG_mpm")
  expect_equal(length(fmp$mpm[[1]]), 8)
})

