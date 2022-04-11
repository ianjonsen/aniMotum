context("test grab")

## generate ssm obj as quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
##  older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(verbose = 0))

p <- grab(xs, what = "fitted", as_sf = TRUE)
test_that("p has s3 classes `sf`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("sf","tbl_df","tbl","data.frame"))
})

p <- grab(xs, what = "predicted", as_sf = TRUE)
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("sf","tbl_df","tbl","data.frame"))
})

p <- grab(xs, what = "predicted")
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","tbl","data.frame"))
})

p <- grab(xs, what = "fitted")
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","tbl","data.frame"))
})