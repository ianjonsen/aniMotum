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