context("test grab")

## grab returns either a sf object or a tibble
data(fit)

p <- grab(fit, what = "fitted")
test_that("p has s3 classes `sf`, `fitted`, `data.frame`", {
  expect_s3_class(p, c("sf","fitted","data.frame"))
})

p <- grab(fit, what = "predicted")
test_that("p has s3 classes `sf`, `predicted`, `data.frame`", {
  expect_s3_class(p, c("sf","predicted","data.frame"))
})

p <- grab(fit, what = "predicted", as_sf = FALSE)
test_that("p has s3 classes `tbl_df`, `predicted`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","predicted","tbl","data.frame"))
})
