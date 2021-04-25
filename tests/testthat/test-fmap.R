context("test fmap")

## generate fG_ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))

mp <- fmap(xs, what = "fitted")
test_that("mp with fitted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, what = "predicted")
test_that("mp with predicted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, what = "predicted", obs = TRUE)
test_that("mp with obs plotted has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, what = "predicted", 
           crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
test_that("mp with new crs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, what = "predicted", ext.rng = c(0.1, 0.05))
test_that("mp with extended plot range has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, what = "predicted", size = 1.5)
test_that("mp with resized points has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, what = "predicted", obs = TRUE, 
           crs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs", 
           ext.rng=c(0.025, 0.075), size = c(1.5, 0.8))
test_that("mp with all arg's exercised has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs[1,], what = "p")
test_that("mp with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})

mp <- fmap(xs, xm, what = "p")
test_that("mp with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(mp, c("gg","ggplot"))
})
