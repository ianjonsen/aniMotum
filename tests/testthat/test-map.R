context("test map")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, 
              model = "mp", 
              time.step=72, 
              control = ssm_control(verbose = 0))

test_that("m with fitted locs has s3 classes `gg`, `ggplot`", {
  m <- map(xs, what = "fitted", silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with predicted locs has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, what = "predicted", silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with obs plotted has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, what = "predicted", aes = aes_lst(obs = TRUE), silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with new crs has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, what = "predicted", 
           crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs", 
           silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with extended plot range has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, what = "predicted", ext.rng = c(0.1, 0.05), silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with resized points has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, what = "predicted", by.date = TRUE, silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with all arg's exercised has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, what = "predicted", aes = aes_lst(obs = TRUE), 
           crs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=85 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs", 
           ext.rng=c(0.025, 0.075), by.date = TRUE, silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})

test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs[1,], what = "p", silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})

xs <- fit_ssm(ellie, model = "crw", time.step = 72, 
              control = ssm_control(verbose = 0))
xm <- fit_mpm(xs, what = "p", model = "mpm")

test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  m <- map(xs, xm, what = "p", silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})


test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  skip_on_cran()
  m <- map(xs, xm, what = "p", aes = aes_lst(line = TRUE, 
                                             mp_pal = hcl.colors(n=100, "RdBu")), 
           silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})

test_that("m with 1 track has s3 classes `gg`, `ggplot`", {
  skip_on_cran()  
  m <- map(xs, xm, what = "p", map_type = "cartolight", silent=TRUE)
  expect_s3_class(m, c("gg","ggplot"))
})
