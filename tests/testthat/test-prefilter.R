context("test prefilter")

data(ellie)
ellie_sf <- sf::st_as_sf(ellie, coords = c("lon","lat"), crs = 4326)

## test run prefilter on sf version of daata
f <- prefilter(ellie_sf, vmax=10, ang=-1, min.dt=120)
test_that("prefilter handles incoming sf data", {
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","keep","obs.type","amf_x","amf_y","geometry"))
})
