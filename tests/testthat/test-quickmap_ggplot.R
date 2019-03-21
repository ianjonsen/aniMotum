context("test quickmap")

data(testfit)
## step through the options incrementally
p <- quickmap(testfit$ssm[[1]], what = "predicted")
test_that("quickmap defaults return a ggplot object", {
  expect_s3_class(p, "ggplot")
})

p <- quickmap(testfit$ssm[[1]], what = "predicted", obs = TRUE)
test_that("quickmap with obs plotted returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

p <- quickmap(testfit$ssm[[1]], what = "predicted", obs = TRUE, outlier = TRUE)
test_that("quickmap w all obs plotted returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

p <- quickmap(testfit$ssm[[1]], what = "predicted", obs = TRUE, outlier = TRUE, ext.rng=c(0.1, 0.1))
test_that("quickmap with plot range extended by 10% in x,y returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

p <- quickmap(testfit$ssm[[1]], what = "predicted", obs = TRUE, outlier = TRUE, ext.rng=c(0.1, 0.1), size = 2.5)
test_that("quickmap with re-sized plot symbols returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

p <- quickmap(testfit$ssm[[1]], what = "predicted", obs = TRUE, outlier = TRUE, ext.rng=c(0.1, 0.1), size = 2.5, crs = "+init=epsg:3031 +lon_0=85")
test_that("quickmap with user-specified crs returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

data(ellie)
test_that("quickmap returns error when input is not a foieGras object or output from grab()", {
  expect_error(quickmap(ellie))
})


