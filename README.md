
<!-- README.md is generated from README.Rmd. Please edit that file -->

# foieGras <a href='https://ianjonsen.github.io/foieGras/index.html'><img src='inst/logo/foieGras_logo.png' align="right" height="300" /></a>

#### fit latent variable movement models to animal tracking data for location quality control and behavioural inference

<!-- badges: start -->

[![foieGras status
badge](https://ianjonsen.r-universe.dev/badges/foieGras)](https://ianjonsen.r-universe.dev)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/foieGras)](https://cran.r-project.org/package=foieGras/)
[![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/foieGras?color=brightgreen)](https://cran.r-project.org/package=foieGras/)
[![Coverage
status](https://codecov.io/gh/ianjonsen/foieGras/branch/master/graph/badge.svg)](https://codecov.io/github/ianjonsen/foieGras?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2628481.svg)](https://doi.org/10.5281/zenodo.2628481)
![R-CMD-check](https://github.com/ianjonsen/foieGras/actions/workflows/check-full.yaml/badge.svg?branch=master)
<!-- badges: end -->

{foieGras} is an R package that fits continuous-time models in
state-space form to filter error-prone animal movement data obtained via
the Argos satellite system, and to estimate changes in movement
behaviour. Template Model Builder {TMB} is used for fast estimation.
Argos data can be either (older) Least Squares-based locations, (newer)
Kalman Filter-based locations with error ellipse information, or a
mixture of the two. The state-space models estimate two sets of location
states: 1) corresponding to each observation, which are usually
irregularly timed (fitted states); and 2) corresponding to (usually)
regular time intervals specified by the user (predicted states).
Locations are returned as both LongLat and on the Mercator projection
(units=km). The models may be applied with appropriate caution to
tracking data obtained from other systems, such as light-level
geolocations and GPS.

## Installation

First, ensure you have R version \>= 3.6.0 installed (preferably R 4.1.0
or higher):

``` r
R.Version()
```

### From R-Universe

As of `v1.0-5`, `{foieGras}` is available via R-Universe. This is where
the latest stable version can always be found. Installation is simple:

``` r
# install from my R-universe repository
install.packages("foieGras", 
                 repos = "https://ianjonsen.r-universe.dev")
```

However, this will not install any of `{foieGras}`’s Suggested packages,
which add extra functionality such as path re-routing around land. To
ensure all Suggested packages, either from R-Universe or CRAN are also
installed:

``` r
install.packages("foieGras", 
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = "Suggests")
```

`install.packages` will tell you that both a binary (CRAN, v0.7-6) and a
source (R-Universe, v1.0-5) version exist but the source version is
newer. Answer `Yes` to install the source version, provided you have the
appropriate compiler tools available (See **From GitHub (source)**,
below). Eventually, R-Universe will provide binary versions of the
latest `{foieGras}` for your platform and compiling from source won’t be
required.

To avoid doing the above every time you want to re-install `{foieGras}`,
you can add my R-Universe repo to your local list of repositories for
package download in your `.Rprofile`. This ensure `install.packages`
automatically grabs the latest version of `{foieGras}`

``` r
#install.packages("usethis")
usethis::edit_r_profile()

# add the following text or replace existing repos option
options(repos = c(ianjonsen = 'https://ianjonsen.r-universe.dev',
                  CRAN = 'https://cloud.r-project.org'))
```

### From CRAN

`{foieGras}` `v0.7-6` is on
[CRAN](https://cran.r-project.org/package=foieGras/) and can be
downloaded within `R`, in the usual way `install.packages("foieGras")`
or, more completely:
`install.packages("foieGras", depedencies = c("Imports","LinkingTo","Suggests"))`.
{foieGras} `v1.0-5` might become available on CRAN.

### From GitHub (source)

To get the very latest but likely unstable `{foieGras}` version, you can
install from GitHub.

On PC’s running Windows, ensure you have installed
[Rtools](https://cran.r-project.org/bin/windows/Rtools/)

On Mac’s, ensure you have installed the [Command Line Tools for
Xcode](https://developer.apple.com/download/all/) by executing
`xcode-select --install` in the terminal; or you can download the latest
version from the URL (free developer registration may be required). A
full Xcode install uses up a lot of disk space and is not required.
Also, ensure you have a suitable Gnu Fortran compiler installed (e.g.,
<https://github.com/fxcoudert/gfortran-for-macOS/releases>).

``` r
remotes::install_github("ianjonsen/foieGras@staging")
```

Note: there can be issues getting compilers to work properly, especially
on M1 Macs. Often, this is due to missing or incorrect Xcode Command
Line Tools and/or Fortran compiler. If you encounter install and compile
issues, you may find a solution in the excellent documentation here
[glmmTMB](https://github.com/glmmTMB/glmmTMB). Alternatively, if you
don’t care to have the very latest version then installing from
R-Universe is the preferred approach. R-Universe automatically builds
binary package versions for all common platforms, so you won’t have to
worry about compiler software or issues!

## Usage

`{foieGras}` is intended to be as easy to use as possible. Here’s a
simple example showing how to fit a move persistence model to Argos
tracking data and visualise the result:

``` r
library(foieGras)

fit <- fit_ssm(sese, 
               vmax= 4, 
               model = "mp", 
               time.step = 24, 
               control = ssm_control(verbose = 0))

plot(fit, type = 3, pages = 1, ncol = 2)
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r
map(fit, 
    what = "predicted", 
    normalise = TRUE,
    crs = "+proj=stere +lon_0=68 +units=km +datum=WGS84")
```

<img src="man/figures/README-explots2-1.png" width="100%" /> Southern
elephant seal silhouettes kindly provided by:  
- female southern elephant seal, Sophia Volzke
([@SophiaVolzke](https://twitter.com/SophiaVolzke), University of
Tasmania)  
- male southern elephant seal, Anton Van de Putte
([@AntonArctica](https://twitter.com/Antonarctica), Université Libre de
Bruxelles)

## What to do if you encounter a problem

If you are convinced you have encountered a bug or
unexpected/inconsistent behaviour when using `{foieGras}`, you can post
an issue [here](https://github.com/ianjonsen/foieGras/issues). First,
have a read through the posted issues to see if others have encountered
the same problem and whether a solution has been offered. You can reply
to an existing issue if you have the same problem and have more details
to share or you can submit a new issue. To submit an issue, you will
need to *clearly* describe the unexpected behaviour, include a
reproducible example with a small dataset, clearly describe what you
expected to happen (but didn’t), and (ideally) post a few
screenshots/images that nicely illustrate the problem.

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

## Acknowledgements

Development of this R package was funded by a consortium of partners
including:  
- **Macquarie University**  
- **US Office of Naval Research** (ONR Marine Mammal Biology;grant
N00014-18-1-2405)  
- Australia’s **Integrated Marine Observing System** (IMOS)  
- Canada’s **Ocean Tracking Network** (OTN)  
- **Taronga Conservation Society**  
- **Birds Canada**  
- **Innovasea/Vemco**  
Additional support was provided by France’s Centre de Synthèse et
d’Analyse sur la Biodiversite, part of the Fondation pour la Recherche
sur la Biodiversité.

Example southern elephant seal data included in the package were sourced
from the IMOS Animal Tracking Facility. IMOS is a national collaborative
research infrastructure, supported by the Australian Government and
operated by a consortium of institutions as an unincorporated joint
venture, with the University of Tasmania as Lead Agent. IMOS supported
elephant seal fieldwork on Iles Kerguelen conducted as part of the IPEV
program No 109 (PI H. Weimerskirch) and the SNO-MEMO program (PI C.
Guinet). SMRU SRDL-CTD tags were partly funded by CNES-TOSCA and IMOS.
All tagging procedures were approved and executed under University of
Tasmania Animal Ethics Committee guidelines.

Animal silhouettes used in the `foieGras` logo were obtained and
modified from sources:  
- southern elephant seal, Anton Van de Putte
([@AntonArctica](https://twitter.com/Antonarctica), Université Libre de
Bruxelles)  
- humpback whale, Chris Huh via [Phylopic.org](http://phylopic.org)
Creative Commons Attribution-ShareAlike 3.0 Unported  
- mallard duck, Maija Karala via [Phylopic.org](http://phylopic.org)
Creative Commons Attribution-ShareAlike 3.0 Unported  
- leatherback turtle, James R. Spotila & Ray Chatterji via
[Phylopic.org](http://phylopic.org) Public Domain Dedication 1.0  
- white shark, Margo Michaud via [Phylopic.org](http://phylopic.org)
Public Domain Dedication 1.0  
- king penguin, Steven Traver via [Phylopic.org](http://phylopic.org)
Public Domain Dedication 1.0
