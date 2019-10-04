context("test grab")

## grab returns either a sf object or a tibble
data(fssm)

p <- grab(fssm, what = "fitted")
test_that("p has s3 classes `sf`, `fitted`, `data.frame`", {
  expect_s3_class(p, c("sf","fitted","data.frame"))
})

p <- grab(fssm, what = "predicted")
test_that("p has s3 classes `sf`, `data.frame`", {
  expect_s3_class(p, c("sf","data.frame"))
})

p <- grab(fssm, what = "predicted", as_sf = FALSE)
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","tbl","data.frame"))
})

data(fmpm)
p <- grab(fmpm, what = "fitted", as_sf = FALSE)
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","tbl","data.frame"))
})