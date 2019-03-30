context("test fit_ssm")

## fit_ssm output is a compound tibble
##  tests expect that fit$ssm are 13-element lists (if optimiser does not crash)
##  that have S3 class foieGras
data(ellie)
dkf <- ellie[seq(1, nrow(ellie), by=6), ]
## drop KF error ellipse info to exercise LS portions of code
dls <- dkf[, 1:5]

## step through prefilter-specific arguments first
## minimum specified arguments - crw
f <- fit_ssm(dkf, model = "crw", time.step = 24)
test_that("fit_ssm defaults + crw + KF return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

f <- fit_ssm(dls, model = "crw", time.step = 24)
test_that("fit_ssm defaults + crw + LS return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## minimum specified arguments - rw + KF
f <- fit_ssm(dkf, model = "rw", time.step=24)
test_that("fit_ssm defaults + rw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## minimum specified arguments - rw + LS
f <- fit_ssm(dls, model = "rw", time.step=24)
test_that("fit_ssm defaults + rw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## low vmax - crw
f <- fit_ssm(dkf, vmax=5, model = "crw", time.step=24)
test_that("fit_ssm vmax + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## low vmax, ang=c(15,25) - crw
f <- fit_ssm(dkf, vmax=5, ang=c(15,25), model = "crw", time.step=24)
test_that("fit_ssm vmax,ang + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## low vmax, ang=c(15,25), min.dt=120 - crw
f <- fit_ssm(dkf, vmax=5, ang=c(15,25), min.dt=120, model = "crw", time.step=24)
test_that("fit_ssm vmax,ang,min.dit + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## turn speed filter off - crw
f <- fit_ssm(dkf, spdf=FALSE, model = "crw", time.step=24)
test_that("fit_ssm no spd filter + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## step through some SSM arguments
## optim="optim" - crw
f <- fit_ssm(dkf, optim="optim", model = "crw", time.step=24)
test_that("fit_ssm optim + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## fit to full data - crw
f <- fit_ssm(dkf, verbose=0, fit.to.subset=FALSE, model = "crw", time.step=24)
test_that("fit_ssm full data + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## turn trace on - crw
f <- fit_ssm(dkf, verbose=2, model = "crw", time.step=24)
test_that("fit_ssm verbose + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

# fiddle w inner control list - crw
f <- fit_ssm(dkf, inner.control=list(maxit=200), model = "crw", time.step=24)
test_that("fit_ssm inner.control + crw return foieGras list w 13 elements", {
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})

## test fit to multiple individuals
data(rope)
f <- fit_ssm(rope, vmax=20, model="crw", time.step=6)
test_that("fit_ssm multi-track returns foieGras lists w 13 elements", {
  expect_equal(nrow(f), 3)
  expect_s3_class(f$ssm[[1]], "foieGras")
  expect_equal(length(f$ssm[[1]]), 13)
})
