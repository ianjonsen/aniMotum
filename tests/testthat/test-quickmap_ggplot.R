context("test quickmap")

## quickmap output is a ggplot objects
data(ellie)
d <- ellie[seq(1, nrow(ellie), by = 5),]
fit <- fit_ssm(d, model = "crw", time.step = 24)

## step through the options incrementally
p <- quickmap(fit$ssm[[1]], what = "predicted")
test_that("quickmap defaults return a ggplot object", {
  expect_s3_class(p, "ggplot")
})

## turn on obs
p <- quickmap(fit$ssm[[1]], what = "predicted", obs = TRUE)
test_that("quickmap with obs plotted returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

## include extreme outliers in obs
p <-
  quickmap(fit$ssm[[1]],
           what = "predicted",
           obs = TRUE,
           outlier = TRUE)
test_that("quickmap w all obs plotted returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

## extend plot range by 10% on both axes
p <-
  quickmap(
    fit$ssm[[1]],
    what = "predicted",
    obs = TRUE,
    outlier = TRUE,
    ext.rng = c(0.1, 0.1)
  )
test_that("quickmap with plot range extended by 10% in x,y returns a ggplot object",
          {
            expect_s3_class(p, "ggplot")
          })

## increase plot symbol size
p <-
  quickmap(
    fit$ssm[[1]],
    what = "predicted",
    obs = TRUE,
    outlier = TRUE,
    ext.rng = c(0.1, 0.1),
    size = 2.5
  )
test_that("quickmap with re-sized plot symbols returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})

## specify CRS for reprojection
p <-
  quickmap(
    fit$ssm[[1]],
    what = "predicted",
    obs = TRUE,
    outlier = TRUE,
    ext.rng = c(0.1, 0.1),
    size = 2.5,
    crs = "+init=epsg:3031 +lon_0=85"
  )
test_that("quickmap with user-specified crs returns a ggplot object", {
  expect_s3_class(p, "ggplot")
})
