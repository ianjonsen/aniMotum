context("test osar")
skip_on_cran()
data(xs)
r <- osar(xs[1,])

test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(r, "qq"))
})

test_that("plot returns a warning for deprecated plot type", {
  expect_warning(plot(r, type = "hist"))
})

test_that("plot completes silently",{
  expect_silent(plot(r, "ts"))
})

test_that("plot completes silently",{
  expect_silent(plot(r, "acf"))
})