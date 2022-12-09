context("test join")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(verbose = 0))
xm <- fit_mpm(xs, model = "mpm")
p <- join(xs, xm, as_sf = TRUE)

test_that("p has s3 classes `ssmmpm`, `sf`, `data.frame`", {
  expect_s3_class(p, c("ssmmpm", "sf", "data.frame"))
})

p <- join(xs, xm)
test_that("p has s3 classes `ssmmpm`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("ssmmpm", "tbl_df", "tbl", "data.frame"))
})

test_that("join catches non ssm class in ssm input", {
  expect_error(join(grab(xs, "p"), xm), "ssm must be an aniMotum ssm fit object with class `ssm_df`")
})

test_that("join catches non mpm class in mpm input", {
  expect_error(join(xs, grab(xm, "f")), "mpm must be an aniMotum mpm fit object with class `mpm_df`")
})
