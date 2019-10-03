context("test join")

## join returns either a sf object or a tibble
data(ssm_fits)
data(mpm_fits)

p <- join(ssm_fits, mpm_fits)
test_that("p has s3 classes `fG_ssmp`, `sf`, `data.frame`", {
  expect_s3_class(p, c("fG_ssmp", "sf", "data.frame"))
})

p <- join(ssm_fits, mpm_fits, as_sf = FALSE)
test_that("p has s3 classes `fG_ssmp`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("fG_ssmp", "tbl_df", "tbl", "data.frame"))
})