context("test prefilter")

data(ellie)
ellie_sf <- sf::st_as_sf(ellie, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## test run prefilter on sf version of daata
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
f <- dplyr::select(f, id, date, lc, smaj, smin,eor,keep,obs.type,emf.x,emf.y,geometry)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","emf.x","emf.y","geometry"))
})

## test on sf data with epsg 3031
ellie_sf <- sf::st_transform(ellie_sf, 
                             crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
f <- dplyr::select(f, id, date, lc, smaj, smin,eor,keep,obs.type,emf.x,emf.y,geometry)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","emf.x","emf.y","geometry"))
})

## test that prefilter converts sf w units=m to units=km
ellie_sf <- sf::st_transform(ellie_sf, 
                             crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
f <- dplyr::select(f, id, date, lc, smaj, smin,eor,keep,obs.type,emf.x,emf.y,geometry)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","emf.x","emf.y","geometry"))
})

## test prefilter catches data that does not have 5 or 8 columns
test_that("prefilter handles incoming sf data", {
  expect_error(prefilter(ellie_sf[, -5], vmax=10, ang=-1, min.dt=120))
}


)
