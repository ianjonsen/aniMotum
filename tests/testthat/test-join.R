context("test join")

## join returns either a sf object or a tibble
data(fs)
data(fm)

p <- join(fs, fm)
test_that("p has s3 classes `fG_ssmp`, `sf`, `data.frame`", {
  expect_s3_class(p, c("fG_ssmp", "sf", "data.frame"))
})

p <- join(fs, fm, as_sf = FALSE)
test_that("p has s3 classes `fG_ssmp`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(p, c("fG_ssmp", "tbl_df", "tbl", "data.frame"))
})

test_that("join catches non fG_ssm class in ssm input", {
  expect_error(join(grab(fs, "p"), fm, as_sf = FALSE), "ssm must be a foieGras ssm fit object with class `fG_ssm`")
})

test_that("join catches non fG_mpm class in mpm input", {
  expect_error(join(fs, grab(fm, "f"), as_sf = FALSE), "mpm must be a foieGras mpm fit object with class `fG_mpm`")
})

test_that("join catches unequal rows in input", {
  expect_error(join(fs[1,], fm, as_sf = FALSE), "number of rows in ssm is NOT equal to number of rows in mpm")
})