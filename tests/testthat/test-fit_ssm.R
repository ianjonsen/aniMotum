context("test fit_ssm")

## fit_ssm output is a compound tibble
##  tests expect that fit$ssm are 13-element lists (if optimiser does not crash)
##  that have S3 class aniMotum
data(ellie)
dkf <- ellie
## drop KF error ellipse info to exercise LS portions of code
dls <- dkf[, 1:5]

## error catch on wrong vmax
test_that("fit_ssm catches vmax error & returns appropriate msg", {
  expect_error(fit_ssm(dkf, vmax = TRUE, model = "crw", time.step = 48, control = ssm_control(verbose = 0)), 
               "vmax must be a positive, non-zero value representing an upper speed threshold in m/s",
               fixed = TRUE)
})

## error catch on wrong ang
test_that("fit_ssm catches ang error & returns appropriate msg", {
  expect_error(fit_ssm(dkf, ang = 10, model = "crw", time.step = 48, control = ssm_control(verbose = 0)), 
               "ang must be either a vector of c(min, max) angles in degrees defining extreme steps to be removed from trajectory, or NA",
               fixed = TRUE)
})

## error catch on wrong distlim
test_that("fit_ssm catches distlim error & returns appropriate msg", {
  expect_error(fit_ssm(dkf, distlim = 1000, model = "crw", time.step = 48, control = ssm_control(verbose = 0)), 
               "distlim must be either a vector of c(min, max) in m defining distances of extreme steps to be removed from trajectory, or NA",
               fixed = TRUE)
})

## error catch on wrong min.dt
test_that("fit_ssm catches min.dt error & returns appropriate msg", {
  expect_error(fit_ssm(dkf, min.dt = FALSE, model = "crw", time.step = 72, control = ssm_control(verbose = 0)), 
               "min.dt must be a positive, numeric value representing the minimum time difference between observed locations in s",
               fixed = TRUE)
})

## step through prefilter-specific arguments first
## minimum specified arguments - crw
test_that("fit_ssm defaults + crw + KF return aniMotum list w 15 elements", {
  f <- fit_ssm(dkf, model = "crw", time.step = 72, control = ssm_control(optim = "nlminb", verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})


test_that("fit_ssm defaults + crw + LS return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dls, model = "crw", time.step = 72, control = ssm_control(verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## minimum specified arguments - rw + KF
test_that("fit_ssm defaults + rw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, model = "rw", time.step = 72, map = list(psi = as.factor(NA)), control = ssm_control(verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## minimum specified arguments - rw + LS
test_that("fit_ssm defaults + rw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dls, model = "rw", time.step = 72, map = list(psi = as.factor(NA)), control = ssm_control(verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## low vmax - crw
test_that("fit_ssm vmax + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, vmax=5, model = "crw", time.step = 72, control = ssm_control(optim = "nlminb", verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## low vmax, ang=c(15,25) - crw
test_that("fit_ssm vmax,ang + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, vmax=5, ang=c(15,25), model = "crw", time.step = 72, control = ssm_control(optim = "nlminb", verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## low vmax, ang=c(15,25), min.dt=120 - crw
test_that("fit_ssm vmax,ang,min.dt + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, vmax=5, ang=c(15,25), min.dt=120, model = "crw", time.step = 72, control = ssm_control(optim = "nlminb", verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## turn speed filter off - crw
test_that("fit_ssm no spd filter + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, spdf=FALSE, model = "crw", time.step = 72, control = ssm_control(optim = "nlminb", verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## step through some SSM arguments
## optim="nlminb" - crw
test_that("fit_ssm optim + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, model = "crw", time.step = 72, control = ssm_control(verbose = 1, optim="nlminb"))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## fit to full data - rw
test_that("fit_ssm full data + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, fit.to.subset=FALSE, model = "rw", time.step = 72, control = ssm_control(verbose = 0))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## turn trace on - crw
test_that("fit_ssm verbose + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, model = "crw", time.step = 72, control = ssm_control(optim = "nlminb", verbose=2))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

# fiddle w inner control list - crw
test_that("fit_ssm inner.control + crw return aniMotum list w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, inner.control=list(maxit=200), model = "crw", time.step = 72, control = ssm_control(verbose = 0, optim = "nlminb"))
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})

## specify lower, upper parameter bounds
test_that("fit_ssm multi-track returns aniMotum lists w 15 elements", {
  skip_on_cran()
  f <- fit_ssm(dkf, vmax=4, model="crw", time.step=72, control = 
                 ssm_control(verbose = 0, 
                             lower = list(l_psi = -1),
                             upper = list(l_psi = 1))
               )
  expect_equal(nrow(f), 1)
  expect_s3_class(f$ssm[[1]], "ssm")
  expect_equal(length(f$ssm[[1]]), 15)
  expect_s3_class(f, "ssm_df")
})
