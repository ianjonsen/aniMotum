context("test osar")
skip_on_cran()
data(xs)


test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  skip_on_cran()
  r <- osar(xs[1,])
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})


test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  skip_on_cran()
  r <- osar(xs)
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  skip_on_cran()
  r <- osar(xs[1,])
  expect_silent(plot(r, "qq"))
})

test_that("plot completes silently", {
  skip_on_cran()
  r <- osar(xs[1,])
  expect_silent(plot(r, type = "hist"))
})