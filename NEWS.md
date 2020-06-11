# foieGras 0.2.0

* first release, updates will follow regularly and be documented here

# foieGras 0.2.1

* patch to improve C++ portability

# foieGras 0.2.2

* patch for compatibility with latest sf version

# foieGras 0.4.0

* adds move persistence models for behavioural inference along animal tracks, via `fit_mpm()`
* adds ability to fit SSM's to processed light-level geolocation or GPS data by adding `lonerr, laterr` variables to input data
* adds One-Step-Ahead prediction residuals for evaluating SSM fits, via `osar()` and generic `plot()` method for `osar` output
* adds diagnostic plot functions that handle multi-individual fits, via generic `plot()` method for `fG_ssm` fit objects
* adds `sf`-enabled mapping function, via `fmap()`
* fixes CRAN check errors from last version, where proj4string syntax `+init:epsg=` was not supported in non-PROJ4 emulation mode on some linux platforms

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