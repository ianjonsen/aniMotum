context("test fmap")

## grab returns either a sf object or a tibble
data(fssm)

mp <- fmap(fssm, what = "fitted")
test_that("mp with fitted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm, what = "predicted")
test_that("mp with predicted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm, what = "predicted", obs = TRUE)
test_that("mp with obs plotted has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm, what = "predicted", 
           crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
test_that("mp with new crs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm, what = "predicted", ext.rng = c(0.1, 0.05))
test_that("mp with extended plot range has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm, what = "predicted", size = 1.5)
test_that("mp with resized points has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm, what = "predicted", obs = TRUE, 
           crs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs", 
           ext.rng=c(0.025, 0.075), size = 1.5)
test_that("mp with all arg's exercised has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(fssm[1,], what = "p")
test_that("mp with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})
