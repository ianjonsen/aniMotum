context("test osar")
skip_on_cran()
data(xs)
res <- osar(xs[1,])

test_that("r has s3 classes `fG_osar`, `tbl_df`, `tbl`, `data.frame`", {
  expect_s3_class(res, c("fG_osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  expect_silent(plot(res, "qq"))
})

test_that("plot returns a warning for deprecated plot type", {
  expect_warning(plot(res, type = "hist"))
})

test_that("plot completes silently",{
  expect_silent(plot(res, "ts"))
})

test_that("plot completes silently",{
  expect_silent(plot(res, "acf"))
})

test_that("plot returns a list w class gg, ggplot elements", {
  tp <- plot(res, "qq", pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})