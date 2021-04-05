context("test plot_simfit")

tr <- sim(xs, reps = 2, what = "fitted")

## expect plot is silent
test_that("plot completes silently - fitted", {
  tp <- plot(tr, ncol = 2, pal = "Cividis", rev = TRUE)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

## expect plot is silent
test_that("plot completes silently - fitted", {
  tp <- plot(tr)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})