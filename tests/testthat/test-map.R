context("test map")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, model = "mp", time.step=72, 
              control = ssm_control(verbose = 0))

m <- map(xs, what = "fitted")
test_that("m with fitted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, what = "predicted")
test_that("m with predicted locs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, what = "predicted", aes = aes_lst(obs = TRUE))
test_that("m with obs plotted has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, what = "predicted", 
           crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
test_that("m with new crs has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, what = "predicted", ext.rng = c(0.1, 0.05))
test_that("m with extended plot range has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, what = "predicted", by.date = TRUE)
test_that("m with resized points has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, what = "predicted", aes = aes_lst(obs = TRUE), 
           crs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs", 
           ext.rng=c(0.025, 0.075), by.date = TRUE)
test_that("m with all arg's exercised has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs[1,], what = "p")
test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

xs <- fit_ssm(sese2, model = "crw", time.step = 72, 
              control = ssm_control(verbose = 0))
xm <- fit_mpm(xs, what = "p", model = "jmpm")

m <- map(xs, xm, what = "p")
test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, xm, what = "p", aes = aes_lst(line = TRUE, 
                                           mp_pal = hcl.colors(n=100, "RdBu"))
         )
test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})

m <- map(xs, xm, what = "p", map_type = "cartolight")
test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  expect_s3_class(m, c("gg","ggplot"))
})
