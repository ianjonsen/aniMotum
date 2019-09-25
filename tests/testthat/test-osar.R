context("test osar")

data(fit)

r <- osar(fit)
test_that("r has s3 classes `osar`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(r, c("osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(r))
})
