context("test osar")

data(fitr)

r <- osar(fitr)
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

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(r, "boxplot"))
})