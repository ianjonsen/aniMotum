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
* fixes CRAN check errors from last version, where proj4string syntax "+init:epsg=" was not suppored in non-PROJ4 emulation mode on some linux platforms