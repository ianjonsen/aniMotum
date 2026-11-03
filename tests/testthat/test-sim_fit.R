context("test sim_fit")

## generate ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running
## older GDAL versions (ellie is highly sub-sampled for this purpose)
xs <- fit_ssm(ellie, 
              spdf=FALSE, 
              model = "crw", 
              time.step=72, 
              control = ssm_control(verbose = 0))
trs <- sim_fit(xs, what = "fitted", reps = 2)

test_that("sim_fit returns sim_fit nested tibble with 2 rows", {
  expect_s3_class(trs, "sim_fit")
  expect_s3_class(trs, "crws")
  expect_equal(nrow(trs), 1)
})

test_that("sim_fit returns a tibble including estimated track, rep = 0", {
  expect_equal(trs$sims[[1]]$rep[1], 0)
})

test_that("sim_fit returns sim_fit nested tibble with 2 rows", {
  trs <- sim_fit(xs, what = "predicted", reps = 2)
  expect_s3_class(trs, "sim_fit")
  expect_s3_class(trs, "crws")
  expect_equal(nrow(trs), 1)
})

test_that("sim_fit returns a tibble excluding estimated track, rep = 0", {
  trs <- sim_fit(xs, what = "p", reps = 2, sim_only = TRUE)
  expect_equal(trs$sims[[1]]$rep[1], 1)
})

test_that("sim returns a tibble with class sim", {
  trs <- sim(N=50, model = "mp", error = "kf", tdist = "gamma")
  expect_s3_class(trs, "sim")
  expect_equal(nrow(trs), 50)
})

test_that("sim returns a tibble with class sim", {
  trs <- sim(N=50, model = "mp", error = "kf", tdist = "reg")
  expect_s3_class(trs, "sim")
  expect_equal(nrow(trs), 50)
})

test_that("sim returns a tibble with class sim", {
  trs <- sim(N=50, model = "mp", error = "ls", tdist = "reg")
  expect_s3_class(trs, "sim")
  expect_equal(nrow(trs), 50)
})

test_that("sim returns a tibble with class sim", {
  trs <- sim(N=50, model = "rw", error = "ls", tdist = "reg")
  expect_s3_class(trs, "sim")
  expect_equal(nrow(trs), 50)
})

test_that("sim returns a tibble with class sim", {
  trs <- sim(N=50, model = "crw", error = "ls", tdist = "reg")
  expect_s3_class(trs, "sim")
  expect_equal(nrow(trs), 50)
})
## issue #76: tracks are simulated in the projection the model was fitted in
test_that("sim_fit respects a user-supplied projection", {
  skip_on_cran()
  el <- sf::st_as_sf(ellie, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(3031)
  f <- suppressMessages(suppressWarnings(
    fit_ssm(el, model = "crw", time.step = 24, control = ssm_control(verbose = 0))))
  set.seed(1)
  s <- sim_fit(f, what = "predicted", reps = 2)
  p <- grab(f, "predicted")
  z <- s$sims[[1]]
  ## rep 0 is the predicted track, in the fit's own coordinates
  expect_equal(z$x[z$rep == 0], p$x)
  expect_equal(z$lon[z$rep == 0], p$lon, tolerance = 1e-6)
  expect_equal(sf::st_crs(attr(s, "prj")), sf::st_crs(f$ssm[[1]]$predicted))
  ## simulated tracks stay in the region of the data
  expect_true(all(z$lat[z$rep > 0] > -80 & z$lat[z$rep > 0] < -30))
})

test_that("sim_fit simulates tracks that cross the dateline without jumps", {
  skip_on_cran()
  d <- ellie
  d$lon <- d$lon + (169.5 - min(d$lon))
  d$lon <- ifelse(d$lon > 180, d$lon - 360, d$lon)
  f <- fit_ssm(d, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(2)
  s <- sim_fit(f, what = "predicted", reps = 2)
  z <- s$sims[[1]]
  step <- unlist(lapply(split(z, z$rep), function(r) sqrt(diff(r$x)^2 + diff(r$y)^2)))
  ## no step spans the width of the world
  expect_true(max(step) < 5000)
})

test_that("sim_fit rw tracks with cpf = TRUE return to their start", {
  skip_on_cran()
  f <- fit_ssm(ellie, model = "rw", time.step = 24, control = ssm_control(verbose = 0))
  set.seed(4)
  s <- sim_fit(f, what = "predicted", reps = 1, cpf = TRUE, sim_only = TRUE)
  z <- s$sims[[1]]
  expect_equal(z$x[nrow(z)], z$x[1])
  expect_equal(z$y[nrow(z)], z$y[1])
})
