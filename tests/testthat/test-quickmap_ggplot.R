context("test quickmap")

data(testfit)
p <- quickmap(testfit$ssm[[1]], what = "predicted", obs = TRUE, outlier = FALSE)

test_that("quickmap returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})
