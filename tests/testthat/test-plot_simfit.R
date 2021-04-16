context("test plot_simfit")

trs <- simfit(xs, reps = 2, what = "fitted")

## expect plot is silent
test_that("plot completes silently", {
  tp <- expect_warning(plot(trs, ncol = 2, pal = "Cividis", rev = TRUE))
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

## expect plot is silent
test_that("plot completes silently", {
  tp <- plot(trs)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})