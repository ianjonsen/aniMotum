context("test fmap")

## grab returns either a sf object or a tibble
data(ssm_fits)

mp <- fmap(ssm_fits, what = "fitted")
test_that("mp with fitted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted")
test_that("mp with predicted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted", obs = TRUE)
test_that("mp with obs plotted has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted", crs = "+init=epsg:3031 +lon_0=85")
test_that("mp with new crs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted", ext.rng = c(0.1, 0.05))
test_that("mp with extended plot range has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted", size = 1.5)
test_that("mp with resized points has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted", obs = TRUE, crs=3031, ext.rng=c(0.025, 0.075), size = 1.5)
test_that("mp with all arg's exercised has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(ssm_fits, what = "predicted")
test_that("mp with 3 tracks has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})