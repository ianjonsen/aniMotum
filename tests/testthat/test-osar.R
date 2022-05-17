context("test osar")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)

test_that("r has s3 classes `osar`, `tbl_df`, `tbl`, `data.frame`", {
  skip_on_cran()
  xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=72, 
                control = ssm_control(se = FALSE, verbose = 0))
  res <- osar(xs)
  expect_s3_class(res, c("osar","tbl_df","tbl","data.frame"))
})

## plot residuals
test_that("plot completes silently", {
  skip_on_cran()
  expect_silent(plot(res, "qq"))
})

test_that("plot returns a warning for deprecated plot type", {
  skip_on_cran()
  expect_warning(plot(res, type = "hist"))
})

test_that("plot completes silently",{
  skip_on_cran()
  expect_silent(plot(res, "ts"))
})

test_that("plot completes silently",{
  skip_on_cran()
  expect_silent(plot(res, "acf"))
})

test_that("plot returns a list w class gg, ggplot elements", {
  skip_on_cran()
  tp <- plot(res, "qq", pages = 0, ask = FALSE)
  expect_type(tp, "list")
  expect_s3_class(tp[[1]], c("gg","ggplot"))
})