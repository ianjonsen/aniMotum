context("test osar")

data(fssm)

r <- osar(fssm[1,])
test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})


test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  skip_on_cran()
  r <- osar(fssm)
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(r, "qq"))
})

test_that("plot completes silently", {
  expect_silent(plot(r, type = "hist"))
})