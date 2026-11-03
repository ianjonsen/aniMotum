context("automatic projection selection")

## auto_crs() ----------------------------------------------------------------

test_that("auto_crs keeps Mercator where it was never a problem", {
  ## temperate, narrow band of latitude
  expect_match(auto_crs(c(150, 152, 151), c(-34, -35, -33)), "proj=merc")
  ## tropical
  expect_match(auto_crs(c(-160, -155), c(-5, 8)), "proj=merc")
})

test_that("auto_crs chooses a conformal conic for a wide latitude band", {
  crs <- auto_crs(sese$lon, sese$lat)
  expect_match(crs, "proj=lcc")
  expect_match(crs, "units=km")

  ## standard parallels sit inside the latitude range, one sixth in from
  ## each end
  lr <- range(sese$lat)
  sp <- as.numeric(regmatches(crs,
          gregexpr("(?<=lat_[12]=)[-0-9.]+", crs, perl = TRUE))[[1]])
  expect_length(sp, 2)
  expect_true(all(sp > lr[1] & sp < lr[2]))
})

test_that("auto_crs chooses stereographic beyond 80 degrees and around the pole", {
  expect_match(auto_crs(c(0, 90, 180), c(-84, -86, -88)), "proj=stere")
  expect_match(auto_crs(seq(-180, 180, 20), rep(78, 19)), "proj=stere")
})

test_that("auto_crs falls back to stereographic across the equator", {
  ## a conic needs both standard parallels in one hemisphere
  expect_match(auto_crs(c(30, 35, 40), c(-30, 0, 35)), "proj=stere")
})

test_that("auto_crs handles the antimeridian", {
  crs <- auto_crs(c(175, 179, -179, -175), c(-45, -46, -47, -45))
  lon0 <- as.numeric(sub(".*lon_0=([-0-9.]+).*", "\\1", crs))
  ## central meridian near 180, not near 0
  expect_true(abs(abs(lon0) - 180) < 10)
})

test_that("auto_crs rejects data with no finite locations", {
  expect_error(auto_crs(c(NA, NA), c(NA, NA)), "no finite locations")
})

## fit_ssm(projection = ) ----------------------------------------------------

test_that("projection = 'auto' is applied to unprojected data", {
  skip_on_cran()
  pf <- fit_ssm(sese, vmax = 4, pf = TRUE, projection = "auto")
  expect_match(sf::st_crs(pf)$proj4string, "proj=lcc")
})

test_that("the default is unchanged", {
  skip_on_cran()
  pf <- fit_ssm(sese, vmax = 4, pf = TRUE)
  expect_match(sf::st_crs(pf)$proj4string, "proj=merc")
})

test_that("a supplied proj4 string is used", {
  skip_on_cran()
  pf <- fit_ssm(sese, vmax = 4, pf = TRUE,
                projection = "+proj=stere +lat_0=-90 +lon_0=70 +datum=WGS84 +units=km +no_defs")
  expect_match(sf::st_crs(pf)$proj4string, "proj=stere")
})

## the constraint that matters: an sf object is never re-projected ------------

test_that("an sf object is respected and projection is ignored", {
  skip_on_cran()

  ## data prepared and projected upstream, as ArgosQC does
  d <- sf::st_as_sf(sese, coords = c("lon", "lat"), crs = 4326)
  d <- sf::st_transform(
    d, "+proj=stere +lat_0=-90 +lon_0=70 +datum=WGS84 +units=km +no_defs")

  ## asking for a different projection must not override the supplied one
  expect_message(pf <- fit_ssm(d, vmax = 4, pf = TRUE, projection = "auto"),
                 "sf object")
  expect_match(sf::st_crs(pf)$proj4string, "proj=stere")
  expect_false(grepl("lcc", sf::st_crs(pf)$proj4string))

  ## and the default path leaves it alone too
  pf2 <- fit_ssm(d, vmax = 4, pf = TRUE)
  expect_match(sf::st_crs(pf2)$proj4string, "proj=stere")
  expect_false(grepl("merc", sf::st_crs(pf2)$proj4string))
})

test_that("an sf object in longlat keeps the existing Mercator behaviour", {
  skip_on_cran()
  d <- sf::st_as_sf(sese, coords = c("lon", "lat"), crs = 4326)
  expect_message(pf <- fit_ssm(d, vmax = 4, pf = TRUE, projection = "auto"),
                 "sf object")
  expect_match(sf::st_crs(pf)$proj4string, "proj=merc")
})

test_that("pf_sf_project ignores prj for an sf object but uses it otherwise", {
  skip_on_cran()

  ## sf input: prj must have no effect
  d <- sf::st_as_sf(sese, coords = c("lon", "lat"), crs = 4326)
  d <- sf::st_transform(
    d, "+proj=laea +lat_0=-57 +lon_0=70 +datum=WGS84 +units=km +no_defs")
  d$keep <- TRUE
  out <- pf_sf_project(d, prj = "+proj=lcc +lat_1=-65 +lat_2=-51 +units=km")
  expect_match(sf::st_crs(out)$proj4string, "proj=laea")
})
