context("test format_data & prefilter")

data(ellie)
ellie_sf <- sf::st_as_sf(ellie, coords = c("lon","lat"), crs = 4326)
ellie_no.sf <- as.data.frame(ellie_sf)
ellie_n <- ellie
names(ellie_n)[1:4] <- c("ind", "time", "lq", "long")

## test that format_data catches incorrect names
test_that("format_data catches incorrect names", {
  expect_error(format_data(ellie_n, id = "id", date = "time", lc = "lq", 
                           coord = c("long","lat")))
})

## test that format_data accepts timezone when character dates input
ellie.dt <- ellie %>% mutate(date = as.character(date))
test_that("format_data accepts timezone", {
  expect_no_error(format_data(ellie.dt, tz = "GMT"))
})

test_that("format_data accepts timezone", {
  expect_no_error(format_data(ellie.dt, tz = "America/Halifax"))
})

ellie_n <- ellie_n[, sample(1:ncol(ellie_n))]
test_that("format_data orders variables properly", {
  f <- format_data(ellie_n, id = "ind", date = "time", lc = "lq", 
                   coord = c("long","lat"))
  expect_equal(names(f), c("id","date","lc","lon","lat","smaj","smin","eor","lonerr","laterr"))
})

## test that format_data & prefilter handle sf-tibbles
test_that("prefilter handles incoming sf data", {
  f <- format_data(ellie_sf)
  f <- prefilter(f, vmax=10, ang=c(15,25), min.dt=120)
  expect_s3_class(f, "sf")
  expect_equal(names(f), c("id","date","lc","smaj","smin","eor","lonerr","laterr","keep","obs.type","emf.x","emf.y","geometry"))
})

## test that format_data & prefilter handle sf data.frames
test_that("prefilter handles incoming sf data", {
  f <- format_data(ellie_no.sf)
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

