context("test grab")

## generate fG_ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))

p <- grab(xs, what = "fitted")
test_that("p has s3 classes `sf`, `fitted`, `data.frame`", {
  expect_s3_class(p, c("sf","fitted","data.frame"))
})

p <- grab(xs, what = "predicted")
test_that("p has s3 classes `sf`, `data.frame`", {
  expect_s3_class(p, c("sf","data.frame"))
})

p <- grab(xs, what = "predicted", as_sf = FALSE)
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","tbl","data.frame"))
})

data(xm)
p <- grab(xm, what = "fitted", as_sf = FALSE)
test_that("p has s3 classes `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("tbl_df","tbl","data.frame"))
})