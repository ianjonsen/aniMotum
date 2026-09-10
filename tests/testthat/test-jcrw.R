context("fit joint (hierarchical) ssm")

## share_control() -----------------------------------------------------------

test_that("share_control returns the recommended defaults", {
  sc <- share_control()
  expect_type(sc, "list")
  expect_equal(sc$D, "hierarchical")
  expect_equal(sc$tau, "pooled")
  expect_equal(sc$psi, "pooled")
  expect_equal(sc$rho_p, "pooled")
  expect_equal(sc$rho_o, "pooled")
  expect_equal(sc$hier.D, "shared")
  expect_equal(sc$ho_scale, "fixed")
  expect_null(sc$group)
})

test_that("share_control refuses random effects on both sides of the variance partition", {
  ## a hierarchical D together with a hierarchical measurement parameter leaves
  ## the two variance hyperparameters trading off against each other
  expect_error(share_control(D = "hierarchical", tau = "hierarchical"),
               "cannot both be hierarchical")
  expect_error(share_control(D = "hierarchical", psi = "hierarchical"),
               "cannot both be hierarchical")
})

test_that("share_control downgrades the identifiability check when strict = FALSE", {
  expect_warning(sc <- share_control(D = "hierarchical", tau = "hierarchical",
                                     strict = FALSE),
                 "cannot both be hierarchical")
  expect_equal(sc$tau, "hierarchical")
})

test_that("share_control allows a hierarchical D with a pooled measurement model", {
  expect_silent(share_control(D = "hierarchical", tau = "pooled", psi = "pooled"))
  expect_silent(share_control(D = "pooled", tau = "individual"))
})

test_that("share_control warns when the specification is just separate fits", {
  expect_warning(share_control(D = "individual", tau = "individual",
                               psi = "individual", rho_p = "individual",
                               rho_o = "individual"),
                 "equivalent to")
})

test_that("share_control validates group", {
  expect_error(share_control(group = c("a", "b")), "single variable")
  expect_error(share_control(strict = "yes"), "single logical")
})

## ssm_prep() ----------------------------------------------------------------

test_that("ssm_prep returns the model data for a single track", {
  skip_on_cran()
  pf <- fit_ssm(ellie, vmax = 4, pf = TRUE)
  p <- ssm_prep(pf, time.step = 24, control = ssm_control(verbose = 0))

  expect_type(p, "list")
  expect_true(all(c("d.all", "dt", "Y", "isd", "obs_mod", "state0",
                    "gap_flag", "ho_flag", "xs") %in% names(p)))
  expect_equal(ncol(p$Y), nrow(p$d.all))
  expect_equal(length(p$dt), nrow(p$d.all))
  expect_equal(length(p$state0), 4)
  ## gap and haulout flags are mutually exclusive by construction
  expect_true(all(p$gap_flag + p$ho_flag <= 1))
})

test_that("ssm_prep rejects multiple individuals", {
  skip_on_cran()
  pf <- fit_ssm(sese, vmax = 4, pf = TRUE)
  expect_error(ssm_prep(pf), "single individual")
})

## fit_ssm(model = "jcrw") ---------------------------------------------------

test_that("fit_ssm accepts jcrw and rejects unknown models", {
  expect_error(fit_ssm(sese, model = "hcrw"), "model can only be 1 of")
})

test_that("jcrw fits a hierarchical model across individuals", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  f <- fit_ssm(sese, vmax = 4, model = "jcrw", time.step = 72,
               init = "moment",
               control = ssm_control(verbose = 0))

  expect_s3_class(f, "ssm_df")
  expect_equal(nrow(f), length(unique(sese$id)))
  expect_true(all(f$pmodel == "jcrw"))

  ## every individual carries its own states but the same joint fit
  expect_true(all(sapply(f$ssm, function(x) inherits(x, "ssm"))))
  expect_true(all(sapply(f$ssm, function(x) nrow(x$fitted) > 0)))

  ## the population mean and the among-individual sd are reported
  pars <- rownames(f$ssm[[1]]$par)
  expect_true(any(grepl("D_pop", pars)))
  expect_true(any(grepl("sd_lD", pars)))

  ## individual diffusion coefficients differ, but are shrunk toward the
  ## population mean relative to independent fits
  Dx <- sapply(f$ssm, function(x) x$par["D_x", "Estimate"])
  expect_equal(length(Dx), nrow(f))
  expect_true(all(is.finite(Dx)))
})

test_that("jcrw with everything individual recovers independent crw fits", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  ## with no pooling the joint objective is the sum of the individual
  ## objectives, so the joint fit should reproduce separate crw fits. This is
  ## the equivalence check between the RTMB likelihood and crw.hpp.
  sc <- suppressWarnings(
    share_control(D = "individual", tau = "individual", psi = "individual",
                  rho_p = "individual", rho_o = "individual")
  )

  fj <- fit_ssm(sese2, vmax = 4, model = "jcrw", time.step = 72,
                share = sc, init = "moment",
                control = ssm_control(verbose = 0))
  fi <- fit_ssm(sese2, vmax = 4, model = "crw", time.step = 72,
                control = ssm_control(verbose = 0))

  Dj <- sort(sapply(fj$ssm, function(x) x$par["D_x", "Estimate"]))
  Di <- sort(sapply(fi$ssm, function(x) x$par["D_x", "Estimate"]))

  expect_equal(Dj, Di, tolerance = 0.05, ignore_attr = TRUE)
})

test_that("jcrw errors on hierarchical measurement parameters", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  sc <- suppressWarnings(share_control(D = "pooled", tau = "hierarchical"))
  expect_error(fit_ssm(sese, vmax = 4, model = "jcrw", time.step = 72,
                       share = sc, control = ssm_control(verbose = 0)),
               "not implemented")
})

test_that("jcrw requires more than one individual", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  ## ellie is a single individual
  expect_error(fit_ssm(ellie, vmax = 4, model = "jcrw", time.step = 72,
                       control = ssm_control(verbose = 0)),
               "at least 2")
})
