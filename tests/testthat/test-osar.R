context("test osar")

data(fssm)

r <- osar(fssm[1,])
test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})

r <- osar(fssm)
test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(r, c("fG_osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(r, "qq"))
})

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(r, "hist"))
})
