context("test sim")

## generate fG_ssm obj a quickly as possible
## have to do this to avoid error when calling st_transform on platforms running older GDAL versions (sese2 is highly sub-sampled for this purpose)
xs <- fit_ssm(sese2, spdf=FALSE, model = "crw", time.step=72, 
              control = ssm_control(se = FALSE, verbose = 0))
trs <- simfit(xs, what = "fitted", reps = 2, )

test_that("sim returns fG_simfit nested tibble with 2 rows", {
  expect_s3_class(trs, "fG_simfit")
  expect_s3_class(trs, "fG_crws")
  expect_equal(nrow(trs), 2)
})

test_that("sim returns a tibble including estimated track, rep = 0", {
  expect_equal(trs$sims[[1]]$rep[1], 0)
})

test_that("sim returns fG_simfit nested tibble with 2 rows", {
  trs <- simfit(xs, what = "predicted", reps = 2)
  expect_s3_class(trs, "fG_simfit")
  expect_s3_class(trs, "fG_crws")
  expect_equal(nrow(trs), 2)
})

test_that("sim returns a tibble excluding estimated track, rep = 0", {
  trs <- simfit(xs, what = "p", reps = 2, sim_only = TRUE)
  expect_equal(trs$sims[[1]]$rep[1], 1)
})

test_that("sim returns a tibble with class fG_sim", {
  trs <- sim(N=100, model = "mpm", error = "kf", tdist = "gamma")
  expect_s3_class(trs, "fG_sim")
  expect_equal(nrow(trs), 100)
})

test_that("sim returns a tibble with class fG_sim", {
  trs <- sim(N=100, model = "mpm", error = "kf", tdist = "reg")
  expect_s3_class(trs, "fG_sim")
  expect_equal(nrow(trs), 100)
})

test_that("sim returns a tibble with class fG_sim", {
  trs <- sim(N=100, model = "mpm", error = "ls", tdist = "reg")
  expect_s3_class(trs, "fG_sim")
  expect_equal(nrow(trs), 100)
})

test_that("sim returns a tibble with class fG_sim", {
  trs <- sim(N=100, model = "rw", error = "ls", tdist = "reg")
  expect_s3_class(trs, "fG_sim")
  expect_equal(nrow(trs), 100)
})

test_that("sim returns a tibble with class fG_sim", {
  trs <- sim(N=100, model = "crw", error = "ls", tdist = "reg")
  expect_s3_class(trs, "fG_sim")
  expect_equal(nrow(trs), 100)
})