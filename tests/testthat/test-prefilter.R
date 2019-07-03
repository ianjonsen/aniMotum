context("test prefilter")

data(ellie)
ellie_sf <- sf::st_as_sf(ellie, coords = c("lon","lat"), crs = 4326)

## test run prefilter on sf version of daata
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
f <- dplyr::select(f, id, date, lc, smaj, smin,eor,keep,obs.type,amf_x,amf_y,geometry)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","amf_x","amf_y","geometry"))
})

## test on sf data with epsg 3031
ellie_sf <- sf::st_transform(ellie_sf, crs = 3031)
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
f <- dplyr::select(f, id, date, lc, smaj, smin,eor,keep,obs.type,amf_x,amf_y,geometry)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","amf_x","amf_y","geometry"))
})

## test that prefilter converts sf w units=m to units=km
ellie_sf <- sf::st_transform(ellie_sf, crs = "+init=epsg:3031 + units=m")
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
f <- dplyr::select(f, id, date, lc, smaj, smin,eor,keep,obs.type,amf_x,amf_y,geometry)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","amf_x","amf_y","geometry"))
})

## test prefilter catches data that does not have 5 or 8 columns
test_that("prefilter handles incoming sf data", {
  expect_error(prefilter(ellie_sf[, -5], vmax=10, ang=-1, min.dt=120))
}


)
