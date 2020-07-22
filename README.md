
<!-- README.md is generated from README.Rmd. Please edit that file -->

**foieGras** - fit latent variable movement models to animal tracking
data for location quality control and behavioural inference

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Coverage
status](https://codecov.io/gh/ianjonsen/foieGras/branch/master/graph/badge.svg)](https://codecov.io/github/ianjonsen/foieGras?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/foieGras)](https://cran.r-project.org/package=foieGras)
[![CRAN\_Downloads](http://cranlogs.r-pkg.org/badges/foieGras?color=brightgreen)](http://www.r-pkg.org/pkg/foieGras)
[![CRAN\_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/foieGras?color=brightgreen)](http://r-pkg.org/pkg/foieGras)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2628481.svg)](https://doi.org/10.5281/zenodo.2628481)

master branch:  
[![Build
Status](https://travis-ci.org/ianjonsen/foieGras.svg?branch=master)](https://travis-ci.org/ianjonsen/foieGras)

dev branch:  
[![Build
Status](https://travis-ci.org/ianjonsen/foieGras.svg?branch=dev)](https://travis-ci.org/ianjonsen/foieGras)
<!-- badges: end -->

`foieGras` is an R package that fits a continuous-time model (RW or CRW)
in state-space form to filter Argos (or GLS) satellite location data.
Template Model Builder (`TMB`) is used for fast estimation. Argos data
can be either (older) Least Squares-based locations, (newer) Kalman
Filter-based locations with error ellipse information, or a mixture of
the two. The state-space model estimates two sets of location states: 1)
corresponding to each observation, which are usually irregularly timed
(fitted states); and 2) corresponding to (usually) regular time
intervals specified by the user (predicted states). Locations are
returned as both LongLat and on the Mercator projection (units=km).
Additional models are provided to infer movement behaviour along the
SSM-estimated most-probable track.

## Installation

First, ensure you have R version \>= 3.6.0 installed (preferably R 4.0.0
or higher):

``` r
R.Version()
```

### From CRAN

`foieGras` is on [CRAN](https://cran.r-project.org/package=foieGras) and
can be downloaded within `R`, e.g., `install.packages("foieGras")` or,
more completely: `install.packages("foieGras", depedencies =
c("Imports","LinkingTo","Suggests"))`

### From GitHub (source)

On PC’s running Windows, ensure you have installed
[Rtools](https://cran.r-project.org/bin/windows/Rtools/)

On Mac’s, ensure you have installed
[Xcode](https://developer.apple.com/xcode/) and Xcode developer tools.
If installation is needed, make sure you start Xcode after install to
ensure final setup of developer tools is completed. Both Xcode and Xcode
developer tools can be installed from the [Mac App
Store](https://itunes.apple.com/au/app/xcode/id497799835?mt=12)

To get the very latest `foieGras` stable version, you can install from
GitHub:

``` r
remotes::install_github("ianjonsen/foieGras")
```

Note: there can be issues getting compilers to work properly, especially
on a Mac with OS X 10.13.x or higher. If you encounter install and
compile issues, I recommend you consult the excellent information on the
[glmmTMB](https://github.com/glmmTMB/glmmTMB) GitHub.

## Basic example

`foieGras` is intended to be as easy to use as possible. Here’s an
example showing how to quality-control Argos tracking data and infer a
behavioural index along the estimated animal tracks:

``` r
library(tidyverse)
library(foieGras)

ellies
#> # A tibble: 288 x 5
#>    id        date                lc      lon   lat
#>    <chr>     <dttm>              <fct> <dbl> <dbl>
#>  1 ct36-F-09 2009-02-10 19:42:44 A      70.6 -49.7
#>  2 ct36-F-09 2009-02-11 07:56:36 A      70.2 -50.2
#>  3 ct36-F-09 2009-02-12 01:53:07 A      70.1 -51.1
#>  4 ct36-F-09 2009-02-12 19:06:55 B      69.5 -52.0
#>  5 ct36-F-09 2009-02-13 12:13:19 B      71.0 -53.1
#>  6 ct36-F-09 2009-02-14 01:10:58 B      70.1 -53.4
#>  7 ct36-F-09 2009-02-14 20:47:58 B      70.3 -54.3
#>  8 ct36-F-09 2009-02-15 15:32:13 A      70.3 -55.4
#>  9 ct36-F-09 2009-02-16 05:28:22 B      70.9 -55.9
#> 10 ct36-F-09 2009-02-16 20:29:14 B      70.9 -56.7
#> # … with 278 more rows

fit <- fit_ssm(ellies, vmax = 4, model = "crw", time.step = 24, verbose = 0) ## turn off parameter trace for tidy output
plot(fit, what = "predicted")
```

![](man/figures/README-example-1.png)<!-- -->

``` r

fmp <- fit %>% 
  grab(what = "predicted", as_sf = FALSE) %>%
  select(id, date, lon, lat) %>%
  fit_mpm(model = "jmpm", verbose = 0) ## turn off parameter trace for tidy output

plot(fmp)
```

![](man/figures/README-example-2.png)<!-- -->

``` r

fmap(fit, fmp, what = "predicted", crs = "+proj=stere +lon_0=99 +units=km +ellps=WGS84")
```

![](man/figures/README-example-3.png)<!-- -->

## Learn more

foieGras can do a lot more. New vignettes, highlighting features are
coming…

## What to do if you encounter a problem

If you are convinced you have encountered a bug or
unexpected/inconsistent behaviour when using foieGras, you can post an
issue [here](https://github.com/ianjonsen/foieGras/issues). First, have
a read through the posted issues to see if others have encountered the
same problem and whether a solution has been offered. You can reply to
an existing issue if you have the same problem and have more details to
share or you can submit a new issue. To submit an issue, you will need
to *clearly* describe the unexpected behaviour, include a reproducible
example with a small dataset, clearly describe what you expected to
happen (but didn’t), and (ideally) post a few screenshots/images that
nicely illustrate the problem.

## How to Contribute

Contributions from anyone in the Movement Ecology/Bio-Logging
communities are welcome. Consider submitting a feature request
[here](https://github.com/ianjonsen/foieGras/issues/new/choose) to start
a discussion. Alternatively, if your idea is well-developed then you can
submit a pull request for evaluation
[here](https://github.com/ianjonsen/foieGras/pulls). Unsure about what
all this means but still want to discuss your idea? then have a look
through the GitHub pages of community-built R packages like
[tidyverse/dplyr](https://github.com/tidyverse/dplyr) for examples.

## Code of Conduct

Please note that the foieGras project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
