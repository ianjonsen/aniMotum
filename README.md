# foieGras
get your location ducks all lined up by fitting continuous-time movement models to Least Squares- and/or Kalman Filter-derived Argos location data

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Coverage status](https://codecov.io/gh/ianjonsen/foieGras/branch/master/graph/badge.svg)](https://codecov.io/github/ianjonsen/foieGras?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/foieGras)](https://cran.r-project.org/package=foieGras)
[![CRAN_Downloads](http://cranlogs.r-pkg.org/badges/foieGraas)](http://www.r-pkg.org/pkg/foieGras)
[![CRAN_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/foieGras)](http://r-pkg.org/pkg/foieGras)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2628481.svg)](https://doi.org/10.5281/zenodo.2628481)

master branch:  
[![Build Status](https://travis-ci.org/ianjonsen/foieGras.svg?branch=master)](https://travis-ci.org/ianjonsen/foieGras)  

**foieGras** - fit Continuous-Time Random Walk and Correlated Random Walk state-space models to filter Argos Least Squares or Kalman Filter location data. 

`foieGras` is an R package that fits a continuous-time model (RW or CRW) in state-space form to filter Argos satellite location data. Template Model Builder (`TMB`) is used for fast estimation. The Argos data can be either (older) Least Squares-based locations, (newer) Kalman Filter-based locations with error ellipse information, or a mixture of the two. The model estimates two sets of location states: 1) corresponding to each observation, which are usually irregularly timed (fitted states); and 2) corresponding to (usually) regular time intervals specified by the user (predicted states). Locations are returned as both LongLat and on the Mercator projection (units=km). 

## Help
Read `?fit_ssm` for details and an example of how to use the package or read the vignette in your web-browser for extended details:
```R
browseVignettes(package = "foieGras")
```

## Installation 
First, ensure you have R version >= 3.5.2 installed:

```R
R.Version()
```

### From CRAN  
`foieGras` is on [CRAN](https://cran.r-project.org/package=foieGras) and can be downloaded within `R`, e.g., `install.packages("foieGras")` or, more completely: `install.packages("foieGras", depedencies = c("Imports","LinkingTo","Suggests"))`  

### From GitHub (source)  

On PC's running Windows, ensure you have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/) 

On Mac's, ensure you have installed [Xcode](https://developer.apple.com/xcode/) and Xcode developer tools. If installation is needed, make sure you start Xcode after install to ensure final setup of developer tools is completed. Both Xcode and Xcode developer tools can be installed from the [Mac App Store](https://itunes.apple.com/au/app/xcode/id497799835?mt=12)

To get the very latest `foieGras` stable version, you can install from GitHub:
```R
remotes::install_github("ianjonsen/foieGras", dependencies = c("Imports","LinkingTo","Suggests"))
```

Note: there can be issues getting compilers to work properly, especially on a Mac with OS X 10.13.x or higher. If you encounter install and compile issues, I recommend you consult the excellent information on the [glmmTMB](https://github.com/glmmTMB/glmmTMB) GitHub.
