# foieGras
get your location ducks all lined up by fitting continuous-time movement models to Least Squares- and/or Kalman Filter-derived Argos location data

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)  
[![Coverage status](https://codecov.io/gh/ianjonsen/foieGras/branch/master/graph/badge.svg)](https://codecov.io/github/ianjonsen/foieGras?branch=master)  

master branch:  
[![Build Status](https://travis-ci.org/ianjonsen/foieGras.svg?branch=master)](https://travis-ci.org/ianjonsen/foieGras)  
dev branch:  
[![Build Status](https://travis-ci.org/ianjonsen/foieGras.svg?branch=dev)](https://travis-ci.org/ianjonsen/foieGras)

**foieGras** - fit Continuous-Time Random Walk and Correlated Random Walk state-space models to filter Argos Least Squares or Kalman Filter location data. 

`foieGras` is an R package that fits a continuous-time model (RW or CRW) in state-space form to filter Argos satellite location data. Template Model Builder (`TMB`) is used for fast estimation. The Argos data can be either (older) Least Squares-based locations, (newer) Kalman Filter-based locations with error ellipse information, or a mixture of the two. The model estimates two sets of location states: 1) corresponding to each observation, which are usually irregularly timed (fitted states); and 2) corresponding to (usually) regular time intervals specified by the user (predicted states). Locations are returned as both LongLat and on the Mercator projection (units=km). 

Read `?fit_ssm` for details and an example of how to use the package 

## Installation
First, ensure you have R version >= 3.5.1 installed:

```R
R.Version
```

On PC's running Windows, ensure you have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/) 

On Mac's, ensure you have installed [Xcode](https://developer.apple.com/xcode/) and Xcode developer tools. If installation is needed, make sure you start Xcode after install to ensure final setup of developer tools is completed. Both Xcode and Xcode developer tools can be installed from the [Mac App Store](https://itunes.apple.com/au/app/xcode/id497799835?mt=12)

Next, you will need to install `TMB` and it's dependencies from within R:
```R
install.packages("TMB")
```

Then install `devtools` and it's dependencies and finally install `foieGras` from GitHub:

```R
install.packages("devtools")  
devtools::install_github("ianjonsen/foieGras")
```
