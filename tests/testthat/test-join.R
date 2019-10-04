context("test join")

## join returns either a sf object or a tibble
data(fssm)
data(fmpm)

p <- join(fssm, fmpm)
test_that("p has s3 classes `fG_ssmp`, `sf`, `data.frame`", {
  expect_s3_class(p, c("fG_ssmp", "sf", "data.frame"))
})

p <- join(fssm, fmpm, as_sf = FALSE)
test_that("p has s3 classes `fG_ssmp`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("fG_ssmp", "tbl_df", "tbl", "data.frame"))
})