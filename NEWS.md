# aniMotum (development version)

* adds `plot()` methods for the behavioural models: `plot.mrm_mi()` (`fit_mrm()`) draws time series of movement rate with pooled 95% intervals (`type = "timeseries"`), and tracks with locations coloured by the estimate (`type = "track"`); `plot.hmm_mi()` (`fit_hmm()`) draws histograms of step lengths and turning angles with the fitted state-dependent densities (`type = "densities"`), and tracks coloured by the most probable state (`type = "track"`). Track plots show one map per track (`pages = 0`) or all tracks on one map (`pages = 1`), with land polygons from `rnaturalearth`
* adds `impute_tracks()`: multiple imputation of regular-interval tracks, the shared basis of `fit_hmm()`, `fit_mrm()` and `fit_mpm(reps = )`. Tracks are imputed either from an SSM fit, with `sim_post()`, or directly from Kinéis locations without an SSM: each location is perturbed by a draw from its error radius (the KI error model of `fit_ssm()`), and the perturbed locations are linearly interpolated to a regular time grid. Grid times inside gaps longer than `max_gap` are dropped and the track is split into segments there
* `plot.mpm_df()` rewritten to match the other behavioural-model plots, and used by `plot()` types 3 and 4 for `fit_ssm(model = "mp")` fits. Arguments: `type` (`"timeseries"` or `"track"`), `pages`, `ncol`, `ask`, `pal`, `rev`, `land`, `se`, `normalise`, `group`, and `y` (only for `fit_mpm()` output from earlier versions, which holds no locations: the ssm fit, needed for track maps). Earlier `fit_mpm()` output, including the `g`, `g.se` layout, is still plotted. Changes from the previous `plot.mpm_df()`: i) track maps need no ssm fit; ii) with `type = "track"`, `pages = 1` draws all tracks on one map; iii) track maps are in longitude and latitude with land; iv) `se` is an argument by name only (it was third by position); v) the y-axis label is "move persistence (g)". With `normalise = TRUE`, the 95% interval is rescaled with g (previously it was shifted on the logit scale), so it shows when the interval is wider than the range of g. `plot.ssm_df()` type 4 now draws longitude and latitude maps, not projected x and y.
* adds `fit_mrm()` (experimental): a time-varying movement rate model. The rate (km h^-1/2) is also reported as a speed in km/h at the fit's time step (`speed`: root-mean-square net displacement over one time step divided by the time step, `rate * sqrt(2 / time.step)`), which `plot()` shows; speeds from fits at different time steps are not comparable. Displacements follow a Brownian random walk whose log rate is a continuous-time random walk; the model is fitted to each imputed track, treating its locations as data, and the log rate at each time and the parameters are pooled with Rubin's rules. A one-step version, with latent locations, was not adopted: its joint likelihood is unbounded (locations can collapse while the log rate goes to -Inf), which breaks the Laplace approximation
* `impute_tracks()`, `fit_mpm()`, `fit_mrm()` and `fit_hmm()` gain `vmax`: for Kinéis locations, a speed-distance-angle filter (`trip::sda()`, with `fit_ssm()`'s default spike angles and lengths) drops extreme locations before imputation. Default NULL: no filter. With `vmax = 4`, it drops 471 of 124,748 locations from 34 southern elephant seals, including an lc B location at 147.3 E, 62.0 S that implied 177 and 1,779 km/h into and out of it.
* `fit_mpm()`: a single fit (e.g. `jmpm` to an `ssm_df` fit) with `verbose = 1` shows the number of objective function evaluations and the time elapsed, instead of the parameter trace; more than one fit shows a progress bar, as before.
* `impute_tracks()` gains `crs`: imputed tracks are projected in one conformal projection for all individuals, chosen from the extent of the data by `auto_crs()` (Mercator where its scale varies little, otherwise Lambert conformal conic, polar or oblique stereographic), or the projection given. Previously Kinéis imputations were in global Mercator and SSM imputations in the SSM fit's projection. Kinéis locations are now interpolated in that projected plane rather than in longitude and latitude. `fit_mpm()` gains `crs` and fits every input (imputations, `ssm_df` locations, longitude and latitude data) in that projection; its output records it (`crs`). For 34 southern elephant seals (46 to 69 degrees south) the chosen Lambert conformal conic keeps projected step lengths within 1% of their ground length, against 1.5 to 2.7 times in Mercator, so `sigma_x` and `sigma_y` no longer average over different stretches. `fit_mrm()` and `fit_hmm()` compute steps on the ground, so for them only the interpolation changes.
* move persistence g is now a random walk on its own scale, held within 0 to 1 by a soft barrier (a penalty near zero inside 0 to 1, rising quadratically outside it), instead of a random walk on logit(g), in every move persistence model: `fit_mpm()` (`mpm`, `jmpm`) and `fit_ssm()` (`mp`, and the joint `jmp`). The first g of each track has a diffuse Normal(0.5, 2) prior. This fixes frequent fit failures. i) The random walk on logit(g) had no distribution for its first value, so a track whose g stayed near 0 or 1 had an unidentified level and a singular inner Hessian (`NA/NaN gradient evaluation`); one such track broke a joint fit. ii) The locations' likelihood is quadratic in g, so it is not convex in logit(g): the Laplace approximation's inner problem could have several modes, giving false convergence or a Hessian that was not positive definite. On the scale of g it is convex in `mpm` and `jmpm`. With 20 imputations at 12 h, `fit_mpm(model = "jmpm")` now fits all 20 for 34 southern elephant seals (none before) and for 21 tu126 turtles (none before; 13 with the prior alone). Output columns change: `g` and `g.se` (standard error on the scale of g) replace `logit_g`, `logit_g.se` and `g`; intervals are `g -/+ 1.96 * g.se`, and g and its interval are reported within 0 to 1. `sigma_g` is on the scale of g, not comparable with earlier estimates. `grab()`, `join()`, `map()`, `plot()` and `route_path()` accept fits made before and after the change. Against GPS-measured behaviour on 10 tu126 turtles (Kinéis locations, 12 h, Spearman correlation of daily g with straightness and with daily distance), `fit_mpm` jmpm scores 0.59, 0.56 (logit link 0.62, 0.61, with 5 of 20 imputations failed), mpm 0.62, 0.58 (0.64, 0.60, 14 failed), and `fit_ssm(model = "mp")` 0.59, 0.59 (0.43, 0.38).
* `fit_mpm()` rewritten as one function for all inputs: an `ssm_df` fit, Kinéis locations, or a data frame of locations. It gains `reps`, `time.step`, `max_gap` and `kineis`: with `reps`, the move persistence model is fitted to tracks imputed by `impute_tracks()` (from an SSM fit, or directly from Kinéis locations and their error radii), and `logit_g` and the parameters are pooled with Rubin's rules, instead of treating point estimates as data. Kinéis locations are only fitted this way; without `reps` they give an error. Output, class `mpm_df`, is now a list, like `fit_mrm()` and `fit_hmm()` output: `fitted` (with the track's `lon`, `lat`, `logit_g`, `logit_g.se`, `g` and its 95% interval), `par` (estimates with 95% intervals), `convergence`, `tmb` (the TMB objects, without `reps` only), `model`, `reps`, `source` (`"ssm"`, `"kineis"` or `"data"`), `time.step` and `what`. `grab()`, `join()`, `map()` and `plot()` accept both this and the earlier tibble output. `map()` also takes `fit_mpm()` output as `x`, without an ssm fit, so fits made directly from Kinéis locations can be mapped (no confidence ellipses or observed locations; track lines are broken at data gaps); with an mpm `y` and no `what`, `map()` uses the locations `y` was fitted to. Locations from an `ssm_df` fit are now fitted in projected coordinates (`x`, `y`) rather than longitude and latitude, so g estimates differ slightly from earlier versions. With `model = "mpm"`, Kinéis track segments separated by gaps longer than `max_gap` are fitted separately.
* `fit_mpm()` now divides x and y by one common constant before fitting, instead of a separate constant for each axis. `sigma_x` and `sigma_y` are therefore in the same units and can be compared (previously, at longitude 135, `sigma_x` was shrunk 13 times relative to `sigma_y`). Estimates of `g` are unchanged: each axis has its own sigma, so a per-axis rescaling is absorbed by that sigma
* `fit_hmm()` (experimental): fits an N-state hidden Markov model (gamma step lengths, von Mises turning angles; `hmmTMB` package) to tracks imputed with `impute_tracks()`, from an SSM fit or directly from Kinéis locations, and pools state probabilities and parameters across imputations. States are ordered by mean step length to prevent label switching. Parameters are pooled with Rubin's rules on hmmTMB's working (link) scale, combining the within-imputation standard errors from hmmTMB's `sdreport` with the between-imputation variance, and back-transformed with hmmTMB's inverse links (`par_summary`; working-scale results in `par_working`)
* `fit_mpm()`, `fit_mrm()` and `fit_hmm()` show a progress bar: over individuals for `fit_mpm(model = "mpm")` (in place of the per-iteration parameter trace when `verbose = 1`), and over imputation fits with multiple imputation. `verbose = 0` (or `fit_hmm(progress = FALSE)`) is silent
* `sim_post()` gains `inflate` (default TRUE): draws for Kinéis-only tracks are scaled by the stored standard-error inflation factor, so their spread matches the inflated `x.se` and `y.se`. Its help now states that the simulations include SSM parameter uncertainty (they sample the joint precision of the random and fixed effects)
* on 10 tu126 turtles at 12 h (20 imputations), the daily mean of each behavioural index correlated with GPS-measured daily distance and 5-day straightness (median Spearman): `fit_hmm()` probability of the long-step state 0.80-0.82 and 0.66-0.67; `fit_mrm()` rate 0.75-0.76 and 0.49; `fit_mpm()` g 0.60-0.63 and 0.60-0.64. Imputing from a `crw` fit or directly from Kinéis locations gave the same results. In simulations with known travel and foraging phases, both imputation routes understated the travel-to-forage rate ratio by about 15% at 12 h (2.08 versus 2.46 from error-free positions), because the location error within each imputed track adds to every displacement
* adds a Kinéis (KI) observation model. Locations supplied with a CLS error radius (`format_data(error.radius = )`, default column name `error.radius`, in m) have ground standard deviations `scale_east * r^exponent` and `scale_north * r^exponent`, with `r` the error radius in km. The KI model takes precedence over any error ellipse supplied for the same location, and is available for the `rw`, `crw` and `mp` models (not yet the joint models)
* the KI ground standard deviations are mapped to the model's projected coordinates through the local Jacobian of the projection (computed numerically on the WGS84 ellipsoid), so the observation covariance is correct in any projection, conformal or not
* adds `ssm_control(kineis = )` to set the KI error scales and exponent and whether they are estimated. The provisional defaults (`scale = c(1.061, 0.739)`, `exponent = 1`, fixed) are the root mean square east and north errors, relative to the error radius, of 1,700 Kinéis locations paired with Fastloc GPS fixes (21 olive ridley turtles, IMOS campaign tu126, 2026). Kinéis errors are serially correlated through CLS Kalman smoothing, which is not yet modelled
* the observation error covariance is now computed by one function shared by the `rw`, `crw` and `mp` models (`src/TMB/sub/observation_covariance.hpp`); results are unchanged
* the standard errors of fits to Kinéis-only tracks are now inflated by default, via `ssm_control(kineis = list(se.inflation = ))`. Fitted and predicted standard errors (`x.se`, `y.se`, and `u.se`, `v.se`, `s.se` for `crw`) are multiplied by 1.8 for `crw`, 1.15 for `rw` and 1.75 for `mp` (`x.se` and `y.se` only, not `g`). Point estimates are unchanged. The factors are calibrated at 1 standard error, so `x.se` and `y.se` keep their meaning: 68.3% of x and y errors, relative to Fastloc GPS locations withheld from fits to tu126 Kinéis locations, fall within 1 inflated standard error. Kinéis errors are heavier-tailed than normal, so intervals of 1.96 standard errors contain about 90%, not 95%, of errors. `map()` and `plot()` therefore draw the 95% confidence regions of Kinéis-only tracks with separate 95% factors stored with each fit, so no refit is needed: for ellipses (`se.inflation$ellipse95`) 2.5 for `crw`, 1.65 for `rw` and 2.35 for `mp`; for the time-series intervals of `plot(type = 1)` (`se.inflation$interval95`) 2.4, 1.55 and 2.25 Tracks that mix KI with other observation types are not inflated; `fit_ssm()` reports any such Kinéis tracks. `se.inflation = 1` turns inflation off. The factor used is stored in each fit (`se.inflation`) and reported by `summary()`. Models with serially correlated (random walk and Ornstein-Uhlenbeck) Kinéis errors were less accurate and less well calibrated than inflated standard errors, so are not included
* adds `kineis_inflation()` to estimate the Kinéis standard-error inflation factor (`rw`, `crw` or `mp`) from tracks with both Kinéis and GPS locations. Argos and GPS locations are supplied separately (`argos`, `gps`), as data frames or .csv files, in SMRU diag and gps file format or aniMotum format, and are returned merged and ordered by date within id. The SSM is fitted to the Kinéis locations only, without inflation, and the factor is the one putting the target proportion of x and y errors (default 68.3%, i.e. 1 standard error) within the matching multiple of the inflated standard errors. Returns the factor, a bootstrap interval across tracks, and coverage and factors by track. The result is passed to `ssm_control(kineis = list(se.inflation = ))`
* fixes the 95% confidence ellipses drawn by `map()` and `plot(type = 2)`: their semi-axes were 1.96 standard errors, which contain only 85% of a bivariate normal distribution. They are now `sqrt(qchisq(0.95, 2))` = 2.448 standard errors, so the ellipses are 25% larger than before. This applies to all data types
* fixes an error in `fit_ssm(model = "crw")` when a track has no prediction times (e.g. `time.step` supplied as a data frame without times for that track)
* fixes uninitialised memory in the `crw` along-track speeds: the first speed of each track (fitted and predicted) is now `NA` whether or not `ssm_control(se = TRUE)`
* fixes `fit_ssm(model = "jmp")` and `"jcrw"`: since the Kinéis standard-error inflation added a field to SSM fit objects, `jsfilter()` no longer recognised its per-individual starting fits as complete (it tested `length(fit) == 15`), silently fell back to crude starting values, and the joint fit could fail with an NaN gradient. `jsfilter()`, `grab()` and `route_path()` now test for the optimiser result and `sdreport` instead of the number of list elements, as `fit_ssm()` already did
* `prefilter()` adds the Kinéis projection Jacobian columns (`KIjac.*`) only when there are Kinéis observations, so its output for other data is unchanged

# aniMotum 1.3-0 (03/11/2026)

* adds a joint (hierarchical) move persistence SSM via `fit_ssm(model = "jmp")`, fitting the time-varying move persistence model to all tracks at once (Jonsen et al. 2019). By default, `sigma_g` and the measurement error parameters are pooled, the process variance is hierarchical, and `rho_p` is estimated separately for each individual
* adds a rotation-invariant joint correlated random walk SSM via `fit_ssm(model = "jcrw")`. Movement scale and anisotropy are shared among individuals, while each individual keeps its own orientation. Anisotropy is set via `share_control(aniso = )`
* adds `share_control()` to specify which parameters are pooled, hierarchical or individual-specific in joint fits. Combinations that are not identifiable are refused
* adds `sim_jcrw()` to simulate tracks from the `jcrw` process model with known parameters
* joint fits have class `jssm_df`. `summary()` and `print()` report convergence and AICc once for the joint fit, and separate parameter estimates shared by all individuals from those estimated per individual
* adds a `projection` argument to `fit_ssm()`. The default `"mercator"` is unchanged, `"auto"` chooses a conformal projection suited to the data extent via the new `auto_crs()`, or a proj4 string can be supplied. The projection is chosen once for all individuals
* `fit_ssm()` now always respects the projection of an input `sf` object and never re-projects it
* adds a `Projections` vignette, describing how the choice of projection affects parameter estimates
* adds `smru_haulout()` to flag haulout periods (`ho = 1`) in tracking data, using an SMRU haulout data file
* adds `haulout`, `ho.ref` and `ho.id_fun` arguments to `fit_ssm()`, calling `smru_haulout()` before fitting. An existing `ho` column in the input data is also used
* the `rw`, `crw` and `mp` models now keep locations nearly stationary during haulouts, by scaling the process variance with `ssm_control(ho_scale = )` (default 0.01)
* adds `ssm_control(gap.thresh = )` to flag data gaps longer than `gap.thresh` hours. The `crw` and `mp` models break directional persistence across flagged gaps, which stops implausible looping by `crw` tracks across large gaps. No gaps are flagged by default
* `route_path()` no longer moves locations known to be on land: haulout locations (`ho = 1`), locations within intervals supplied via the new `onland` argument, and on-land locations at track ends. All locations are returned, with `on_land = TRUE` where kept on land
* `sim_fit()` now simulates tracks in the projection used to fit the SSM, rather than assuming Mercator. This fixes simulations from fits to projected `sf` data and from tracks that cross -180,180 (#76)
* `sim_post()` now samples more efficiently, using far less memory for long tracks (Jan-Ole Koslik, #85), supports joint fits, and no longer assumes a Mercator projection
* fixes issue with `osar()` where residuals were assigned to the wrong individuals when they could not be calculated for some individuals (#91)
* fixes issue with `fit_mpm(model = "jmpm")` where tracks with numeric ids and unequal lengths resulted in an error. Also fixes `fit_mpm()` dropping `id` and `date` when coordinates are named `x`, `y`, and `grab()` failing on crashed `jmpm` fits (#90)
* fixes issue with `fit_ssm(model = "mp")` where `tau` was estimated for "GL" locations (#82)
* fixes issue with `fit_ssm()` where all columns but geometry were dropped from `sf` input data when `spdf = FALSE`
* fixes issue with `ellp.par()` where simulated location errors did not match their error ellipses
* fixes issue with `sim_fit()` where simulations used fitted locations when `what = "predicted"` was specified, and `rw` tracks did not return to their start when `cpf = TRUE`
* removes `ggplot2` warnings about empty axis labels in plot functions


# aniMotum 1.2-06 (05/06/2024)

* various minor issues fixed including: build against TMB 1.9.11, fixes error when grad specified in `sim_fit()`.


# aniMotum 1.2 (03/11/2023)

* addition of `sim_post()`, a function for posterior simulations from SSM fits, conditional on data and movement parameters
* addition of plot method for `sim_post` objects
* `sim_filter()` can now use arbitrary variables, including user-appended environmental variables, for filtering tracks simulated with `sim_fit()`
* `sim_fit()` now simulates tracks from user-specified `start` and `end` locations that differ from the estimated track start and end.
* `fit_ssm()` now handles "generic location" data provided the locations have `x` and `y` standard errors. These data can be light-level geolocations, acoustic telemetry positions, or other location data. Input data should have `lc = "GL"` for all generic locations.
* a change to the `min.dt` argument in `fit_ssm()`, the default is now `min.dt = 0` (no minimum time interval between observations, but any subsequent observations that occur at the same time are ignored when fitting an SSM).
* fixed an issue with high temporal resolution data, where prediction times exactly match observation times when observations occur 1 s apart. This caused an error when fitting SSM's.


# aniMotum 1.1-06 (13/07/2023)

* fixes issue with `route_path()` where simulated tracks that are entirely on land resulted in an error


# aniMotum 1.1-04 (01/03/2023)

* fixes issue with `grab()` where multiple data sets with `lon` modulo 0,360 resulted in an error
* fixes issue with `route_path()` where rerouting tracks that have no locations on land resulted in an error. In these cases, `route_path()` now returns a tibble identical to that supplied (fitted or predicted locations) and issues a message on the console.


# aniMotum 1.1-02 (13/02/2023)

* update citations to include new aniMotum R package paper in Methods in Ecology and Evolution as the primary reference.


# aniMotum 1.1-01 (01/02/2023)

* fixes issue with map() where function could become paused one some calls.


# aniMotum 1.1 (10/12/2022)

* package name change to coincide with Methods in Ecology and Evolution manuscript: Jonsen et al. `aniMotum`, an R package for animal movement data: rapid quality control, behavioural estimation and simulation. Accepted 06/12/2022.


# foieGras 1.1 (01/12/2022)

* adds `format_data()` to pre-process non-default data formats into that expected by `fit_ssm`
* adds greater flexibility for input data formats (via new arguments to `fit_ssm`), related to `format_data`
* fixes an issue with input data as an `sf-tibble` or `sf-data.frame` that caused an error when fitting SSM's via `fit_ssm`
* when installing package from source, C++ code is automatically compiled against the existing TMB package version. This should eliminate the warning message on installation and package load that some users may have experienced previously
* updated `Overview` vignette, describing input data structures in greater detail


# foieGras 1.0-5 (15/05/2022)

* adds `route_path()` a wrapper function calling [`pathroutr`](https://github.com/jmlondon/pathroutr) to re-route `fit_ssm` estimated or `simfit` simulated tracks around land barriers
* adds a move persistence model via `fit_ssm(model = "mp")` to allow simultaneous estimation of locations and move persistence. This approach may be preferable to using `fit_mpm()` on a `fit_ssm` model object. `fit_mpm()` is retained for less error-prone (GPS) location data
* adds option for move persistence estimates to be normalised to 0,1 in `grab()`, either on tracks separately or as a group for a relative measure that spans 0 - 1. 
* adds `sim_filter()` to calculate similarity between simulated and ssm-estimated tracks, and returns the most similar simulated tracks based on a user-specified quantile
* adds mapping function `map()` to replace `fmap()` for faster, more flexible estimated track maps & fixes to coastline and other mapping issues for tracks that cross -180,180
* adds rosm map tile layers to `map()` via `ggspatial::annotation_map_tile` for more detailed coastlines on large-scale maps
* adds faster `crw` model fitting via `fit_ssm()` by turning off travel rate standard error (s.se) estimation in `ssm_control()` as the default. SE estimation can be turned on via `control = ssm_control(se = TRUE)`.
* adds a `summary` function for displaying information about SSM fits. 
* replaces hcl.colors("Zissou1") palette for most plots - Zissou1 was fun but not colour blind-friendly; in most cases "Cividis" is now the default but users can specify any `hcl.pals()` palette, using the `pal` argument in many of the plot functions.
* fit object s3 classes `fG_ssm`, `fG_mpm` migrated to `ssm_df`, `mpm_df`
* generic plot method s3 classes migrated to `plot.ssm_df`, `plot.mpm_df`, `plot.osar`, `plot.sim`, `plot.simfit`
* adds new vignettes for more comprehensive documentation of package features


# foieGras 0.7-6 (26/04/2021)

* adds fixes to example data for testing/examples so pkg builds on solaris and various linux boxes with older GDAL/PROJ libraries


# foieGras 0.7-5 (23/04/2021)

* adds `sim()` to simulate animal tracks using the `rw`, `crw` or `mpm` process models. The `rw` and `crw` models can also be specified with state-switching between multiple behavioural states. Tracks can be simulated with or without Argos (LS or KF) errors, as time-regular or time-irregular
* adds `simfit()` to simulate animal tracks from `fit_ssm` fit objects
* adds generic `plot()` methods for `sim` and `simfit` objects
* adds `fit_mpm` can take a `fit_ssm` object directly as input, removing need for user to manipulate data prior to calling `fit_mpm`
* adds `fit_mpm` can fit to SSM-`predicted` (time regular) or SSM-`fitted` (time irregular) locations, via `what` argument
* adds `fit_mpm` can fit to lon,lat or x,y coordinates, via `coords` argument
* adds `ssm_control()` for centralized control over optimizer and optimization method choices, optimizer parameters, and `foieGras` model parameter bounds
* adds faster and more stable optimization
* adds time-series and `acf()` plots as option when visualising prediction residuals calculated from `osar()`
* removes `hist` plots as option when visualising prediction residuals calculated from `osar()`
* removes (with deprecation errors) `verbose`, `optim`, `optMeth`, and `lpsi` arguments to `fit_ssm`
* replaces wesanderson::wes_palette("Zissou1") with hcl.colors("Zissou1") and provides arguments to change this default palette


# foieGras 0.6-9

* adds `wesanderson::wes_palette("Zissou1")` as default palette for plots/maps
* generic plot method for `fG_ssm` objects can now plot individuals all on 1 page (`pages = 1`) or on separate pages (`pages = 0`)
* 2-d (track) plots now include confidence ellipses on estimated locations
* generic plot method for `fG_mpm` objects now available
* mapping function (`fmap`) can optionally take an `fG_mpm` object to colour locations by behavioural index (`gamma_t`)


# foieGras 0.6-7

* adds ability to fit move persistence models to temporally regular OR irregular location data
* adds ability to turn off predicted locations by setting `time.step=NA` causing locations to be estimated only at observation times
* adds ability to fit to Argos - GPS or Argos - GLS locations simultaneously when combined in a single input data.frame
* adds ability to specify an alternate emf (Error Multiplication Factor) `data.frame` for Least-Squares and/or GPS locations
* adds human-readable/understandable errors and warnings with (occasional) advice
* turns off estimation of psi parameter (ellipse semi-minor axis re-scaling factor) when `rw` process model is fit to Argos KF/KS data
* removes reliance on `dplyr::do`, which is superseded as of `dplyr 1.0.0`
* replaces dplyr progressbar with parameter trace when fitting SSM to single or multiple data sets in default verbose mode 1
* replaces `argosfilter::sdafilter` in favour of `trip::sda` (which is a faster, vectorized version of the former) to prefilter outlier locations


# foieGras 0.4.0

* adds move persistence models for behavioural inference along animal tracks, via `fit_mpm()`
* adds ability to fit SSM's to processed light-level geolocation or GPS data by adding `lonerr, laterr` variables to input data
* adds One-Step-Ahead prediction residuals for evaluating SSM fits, via `osar()` and generic `plot()` method for `osar` output
* adds diagnostic plot functions that handle multi-individual fits, via generic `plot()` method for `fG_ssm` fit objects
* adds `sf`-enabled mapping function, via `fmap()`
* fixes CRAN check errors from last version, where proj4string syntax `+init:epsg=` was not supported in non-PROJ4 emulation mode on some linux platforms


# foieGras 0.2.2

* patch for compatibility with latest sf version


# foieGras 0.2.1

* patch to improve C++ portability


# foieGras 0.2.0

* first release, updates will follow regularly and be documented here





