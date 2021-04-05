context("test plot_sim")

## expect plot is silent
tr <- sim(N=100, model = "crw", error = "kf")

test_that("plot completes silently - fitted", {
  tp <- plot(tr, error = TRUE, pal = "Cividis", rev = TRUE, col=TRUE)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

test_that("plot completes silently - fitted", {
  tp <- plot(tr, error = FALSE, rev = TRUE)
  expect_s3_class(tp, c("patchwork", "gg", "ggplot", exact = TRUE))
})

