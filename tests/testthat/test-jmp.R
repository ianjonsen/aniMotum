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

  ## grab() switches on class(x)[1], and jssm_df has to sit ahead of ssm_df
  ## for summary() and print() to dispatch to the joint methods. Without a
  ## fall-through for it, no branch matched and grab() failed with
  ## "object 'out' not found".
  for (w in c("fitted", "predicted", "data")) {
    for (sf in c(TRUE, FALSE)) {
      g <- grab(f, what = w, as_sf = sf)
      expect_true(nrow(g) > 0)
      expect_true(all(c("id", "date") %in% names(g)))
      expect_equal(length(unique(g$id)), nrow(f))
    }
  }

  ## the abbreviated `what` used at the console must work too
  expect_equal(nrow(grab(f, "p", as_sf = FALSE)),
               nrow(grab(f, "predicted", as_sf = FALSE)))

  g <- grab(f, what = "predicted", as_sf = FALSE)
  expect_true("g" %in% names(g))

  ## and a joint fit must give the same state columns as a per-individual mp fit
  fi <- fit_ssm(sese, vmax = 4, model = "mp", time.step = 24,
                control = ssm_control(verbose = 0))
  expect_setequal(names(grab(f, "predicted", as_sf = FALSE)),
                  names(grab(fi, "predicted", as_sf = FALSE)))
})

## a joint fit must not look like several separate fits ----------------------

test_that("a joint fit carries its own classes", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  f <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            control = ssm_control(verbose = 0))
  )

  ## jssm_df ahead of ssm_df, so summary() and print() dispatch to the joint
  ## methods while grab(), plot() and map() inherit the ssm_df behaviour
  expect_s3_class(f, "jssm_df")
  expect_s3_class(f, "ssm_df")
  expect_equal(class(f)[1], "jssm_df")
  expect_true(all(sapply(f$ssm, function(x) inherits(x, "jmp_ssm"))))
  expect_true(all(sapply(f$ssm, function(x) inherits(x, "mp_ssm"))))

  ## a per-individual mp fit must NOT pick up the joint classes
  fi <- fit_ssm(sese, vmax = 4, model = "mp", time.step = 24,
                control = ssm_control(verbose = 0))
  expect_false(inherits(fi, "jssm_df"))
  expect_false(any(sapply(fi$ssm, function(x) inherits(x, "jmp_ssm"))))
})

test_that("the parameter table records which estimates are shared", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  f <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            control = ssm_control(verbose = 0))
  )
  p <- f$ssm[[1]]$par
  sh <- attr(p, "shared")

  expect_type(sh, "logical")
  expect_length(sh, nrow(p))

  ## sigma_g and tau are pooled by default; rho_p is per individual
  expect_true(sh[rownames(p) == "sigma_g"])
  expect_true(sh[rownames(p) == "tau_x"])
  expect_false(sh[rownames(p) == "rho_p"])

  ## and the shared ones really are identical across animals
  sg <- sapply(f$ssm, function(x) x$par["sigma_g", "Estimate"])
  expect_equal(length(unique(round(sg, 12))), 1L)

  ## while rho_p is not
  rp <- sapply(f$ssm, function(x) x$par["rho_p", "Estimate"])
  expect_gt(length(unique(round(rp, 8))), 1L)
})

test_that("summary reports the fit once, not once per animal", {
  skip_on_cran()
  skip_if_not_installed("RTMB")

  f <- suppressWarnings(
    fit_ssm(sese, vmax = 4, model = "jmp", time.step = 24,
            control = ssm_control(verbose = 0))
  )
  s <- summary(f)

  expect_s3_class(s, "summary.jssm_df")
  expect_true(all(c("Fittab", "Stattab", "Shared") %in% names(s)))

  ## the fit is described once
  expect_equal(nrow(s$Fittab), 1L)
  expect_true(all(c("neg.log.lik", "converged", "AICc") %in%
                    colnames(s$Fittab)))

  ## and NOT against each animal, which would invite summing or comparing it
  expect_equal(nrow(s$Stattab), length(unique(sese$id)))
  expect_false("AICc" %in% colnames(s$Stattab))
  expect_false("converged" %in% colnames(s$Stattab))

  ## shared parameters appear once rather than in every individual's table
  expect_true("sigma_g" %in% s$Shared[, "Parameter"])
  if (!is.null(s$Partab))
    expect_false(any(sapply(s$Partab, function(p)
      !is.null(p) && "sigma_g" %in% p[, "Parameter"])))
})
