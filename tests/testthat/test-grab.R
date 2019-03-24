context("test grab")

## grab returns either a sf object or a tibble
data(ellie)
dkf <- ellie[seq(1, nrow(ellie), by = 5),]
f <- fit_ssm(dkf, model = "crw", time.step = 24)

p <- grab(f, what = "fitted")
test_that("p has s3 classes `sf`, `predicted`, `data.frame`", {
  expect_s3_class(p, c("sf","fitted","data.frame"))
})

p <- grab(f, what = "predicted")
test_that("p has s3 classes `sf`, `predicted`, `data.frame`", {
  expect_s3_class(p, c("sf","predicted","data.frame"))
})

p <- grab(f, what = "predicted", as_sf = FALSE)
test_that("p has s3 classes `tbl_df`, `predicted`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","predicted","tbl","data.frame"))
})
