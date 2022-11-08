context("test format_data & prefilter")

data(ellie)
ellie_sf <- sf::st_as_sf(ellie, coords = c("lon","lat"), crs = 4326)

## test run prefilter on sf version of data
test_that("prefilter handles incoming sf data", {
  f <- format_data(ellie_sf)
  f <- prefilter(f, vmax=10, ang=c(15,25), min.dt=120)
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","lonerr","laterr","keep","obs.type","emf.x","emf.y","geometry"))
})

## test on sf data with epsg 3031
ellie_sf <- sf::st_transform(ellie_sf, 
                             crs = sf::st_crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))
test_that("prefilter handles incoming sf data", {
  f <- format_data(ellie_sf)
  f <- prefilter(f, vmax=10, ang=c(15,25), min.dt=120)
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","lonerr","laterr","keep","obs.type","emf.x","emf.y","geometry"))
})

## test that prefilter converts sf w units=m to units=km
ellie_sf <- sf::st_transform(ellie_sf, 
                             crs = sf::st_crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
f <- format_data(ellie_sf)
f <- prefilter(f, vmax=10, ang=c(15,25), min.dt=120)
test_that("prefilter converts m to km for projected data", {
  expect_s3_class(f, "sf")
  expect_true(grepl("units=km", sf::st_crs(f)$input, fixed = TRUE))
})

