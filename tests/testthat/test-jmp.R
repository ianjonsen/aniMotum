context("fit joint (hierarchical) move persistence ssm")

## share_control() -----------------------------------------------------------

test_that("share_control returns the recommended defaults", {
  sc <- share_control()
  expect_type(sc, "list")
  expect_equal(sc$sigma, "hierarchical")
  expect_equal(sc$sigma_g, "pooled")
  expect_equal(sc$rho_p, "individual")
  expect_equal(sc$tau, "pooled")
  expect_equal(sc$psi, "pooled")
  expect_equal(sc$rho_o, "pooled")
  expect_equal(sc$hier.sigma, "shared")
  expect_null(sc$group)
})

test_that("share_control refuses random effects on both sides of the variance partition", {
  expect_error(share_control(sigma = "hierarchical", tau = "hierarchical"),
               "cannot both be hierarchical")
  expect_error(share_control(sigma = "hierarchical", psi = "hierarchical"),
               "cannot both be hierarchical")
})

test_that("share_control downgrades the identifiability check when strict = FALSE", {
  expect_warning(sc <- share_control(sigma = "hierarchical",
                                     tau = "hierarchical", strict = FALSE),
                 "cannot both be hierarchical")
  expect_equal(sc$tau, "hierarchical")
})

test_that("share_control warns when sigma_g is not pooled", {
  ## individual sigma_g puts each animal's g_t on its own smoothing scale,
  ## which defeats the purpose of the joint model
  expect_warning(share_control(sigma_g = "individual"), "not comparable")
})

test_that("share_control allows a hierarchical sigma with a pooled measurement model", {
  expect_silent(share_control())
  expect_silent(share_control(sigma = "pooled", rho_p = "pooled"))
})

test_that("share_control validates group and strict", {
  expect_error(share_control(group = c("a", "b")), "single variable")
  expect_error(share_control(strict = "yes"), "single logical")
})

## ssm_prep() ----------------------------------------------------------------

test_that("ssm_prep returns the model data for a single track", {
  skip_on_cran()
  pf <- fit_ssm(ellie, vmax = 4, pf = TRUE)
  p <- ssm_prep(pf, time.step = 24, control = ssm_control(verbose = 0))

  expect_type(p, "list")
  expect_true(all(c("d.all", "dt", "Y", "isd", "obs_mod", "gap_flag",
                    "ho_flag", "xs", "v.init") %in% names(p)))
  expect_equal(ncol(p$Y), nrow(p$d.all))
  expect_equal(length(p$dt), nrow(p$d.all))
  ## gap and haulout flags are mutually exclusive by construction
  expect_true(all(p$gap_flag + p$ho_flag <= 1))
  ## starting velocities are bounded: dt is floored before dividing
  expect_true(all(is.finite(p$v.init)))
})

test_that("ssm_prep rejects multiple individuals", {
  skip_on_cran()
  pf <- fit_ssm(sese, vmax = 4, pf = TRUE)
  expect_error(ssm_prep(pf), "single individual")
})

## fit_ssm(model = "jmp") ----------------------------------------------------

test_that("fit_ssm accepts jmp and rejects unknown models", {
  expect_error(fit_ssm(sese, model = "jcrw"), "model can only be 1 of")
})

test_that("jmp requires more than one individual", {
  skip_on_cran()
  skip_if_not_installed("RTMB")
  ## ellie is a single individual
  expect_error(fit_ssm(ellie, vmax = 4, model = "jmp", time.step = 72,
                       control = ssm_control(verbose = 0)),
               "at least 2")
})

test_that("jmp errors on hierarchical measurement parameters", {
  skip_on_cran()
  skip_if_not_installed("RTMB")
  sc <- suppressWarnings(share_control(sigma = "pooled", tau = "hierarchical"))
  expect_error(fit_ssm(sese, vmax = 4, model = "jmp", time.step = 72,
                       share = sc, control = ssm_control(verbose = 0)),
               "not implemented")
})

test_that("jmp fits a joint move persistence model across individuals", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  f <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            control = ssm_control(verbose = 0))
  )

  expect_s3_class(f, "ssm_df")
  expect_equal(nrow(f), length(unique(sese$id)))
  expect_true(all(f$pmodel == "jmp"))
  expect_true(all(sapply(f$ssm, function(x) inherits(x, "mp_ssm"))))

  ## move persistence is estimated for every state
  expect_true(all(sapply(f$ssm, function(x) "g" %in% names(x$fitted))))
  gs <- unlist(lapply(f$ssm, function(x) x$fitted$g))
  expect_true(all(gs >= 0 & gs <= 1, na.rm = TRUE))

  ## sigma_g is pooled, so it appears once and is identical for all animals
  sg <- sapply(f$ssm, function(x) x$par["sigma_g", "Estimate"])
  expect_equal(length(unique(round(sg, 10))), 1L)

  ## rho_p is individual by default
  rp <- sapply(f$ssm, function(x) x$par["rho_p", "Estimate"])
  expect_equal(length(rp), nrow(f))
})

test_that("a pooled sigma reproduces the hierarchical fit when there is no among-individual variance", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  ## when sd_lsig goes to its lower bound the hierarchical model is the pooled
  ## model with one extra parameter, so the shared estimates should agree
  fp <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            share = share_control(sigma = "pooled"),
            control = ssm_control(verbose = 0))
  )
  fh <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            control = ssm_control(verbose = 0))
  )

  sd_lsig <- fh$ssm[[1]]$par["sd_lsig", "Estimate"]
  if (sd_lsig < 1e-3) {
    for (nm in c("sigma_g", "tau_x", "tau_y")) {
      expect_equal(fh$ssm[[1]]$par[nm, "Estimate"],
                   fp$ssm[[1]]$par[nm, "Estimate"],
                   tolerance = 1e-3, ignore_attr = TRUE)
    }
  } else {
    succeed("among-individual variance is non-zero; comparison not applicable")
  }
})

test_that("grab and plot methods work on a joint fit", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  f <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            control = ssm_control(verbose = 0))
  )
  g <- grab(f, what = "predicted", as_sf = FALSE)
  expect_s3_class(g, "data.frame")
  expect_true(all(c("id", "date", "g") %in% names(g)))
  expect_equal(length(unique(g$id)), nrow(f))
})
