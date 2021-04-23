##' \pkg{foieGras}
##'
##' fit Continuous-Time Random Walk and Correlated Random Walk state-space models to filter Argos Least Squares or Kalman Filter location data
##'
##' @name foieGras-package
##' @aliases foieGras foieGras-package
##' @docType package
##' @author Ian Jonsen, Toby Patterson
##'
##' @seealso fit_ssm
##' @references Jonsen ID, Patterson TA, Costa DP, et al. (2020) A continuous-time state-space model for rapid quality-control of Argos locations from animal-borne tags. Movement Ecology 8:31 https://doi.org/10.1186/s40462-020-00217-7
##' @references Jonsen ID, McMahon CR, Patterson TA, et al. (2019) Movement responses to environment: fast inference of variation among southern elephant seals with a mixed effects model. Ecology. 100(1):e02566 https://doi.org/10.1002/ecy.2566
##'
##' @keywords foieGras
##' @importFrom assertthat assert_that
##' @importFrom utils data flush.console globalVariables
##' @importFrom dplyr %>% select mutate slice group_by
##' @importFrom dplyr distinct arrange filter left_join lag full_join bind_cols
##' @importFrom dplyr summarise
##' @importFrom furrr future_map
##' @importFrom future availableCores cluster makeClusterPSOCK plan
##' @importFrom tibble as_tibble tibble
##' @importFrom lubridate ymd_hms
##' @importFrom purrr map
##' @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##' @importFrom sf st_coordinates st_geometry<- st_bbox st_cast
##' @importFrom trip sda speedfilter trip
##' @importFrom TMB MakeADFun sdreport newtonOption oneStepPredict
##' @importFrom stats approx cov sd predict nlminb optim na.omit median qlogis qnorm pnorm runif
##' @importFrom utils flush.console globalVariables
##' @importFrom ggplot2 ggplot geom_point geom_path geom_ribbon geom_qq geom_qq_line geom_histogram aes ggtitle theme_bw
##' @importFrom ggplot2 theme element_blank geom_sf xlim ylim unit aes_string
##' @importFrom ggplot2 element_text scale_colour_manual scale_colour_gradientn
##' @importFrom grDevices extendrange grey hcl.colors
NULL

##' @name ellie
##' @docType data
##' @title Southern elephant seal Argos satellite data (1 individual, sub-sampled for testing speed)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name sese
##' @docType data
##' @title Southern elephant seal Argos satellite data (5 individuals)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name sese1
##' @docType data
##' @title Southern elephant seal Argos satellite data (1 individual)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name xs
##' @docType data
##' @title foieGras example fit object
##' @format .RData
##' @keywords data
##' @description Example foieGras fit object. This example
##' fit is included purely to speed up examples where a fit object is required
##' but fitting to data is not the focus of the example.
NULL

##' @name xm
##' @docType data
##' @title foieGras example mpm fit object
##' @format .RData
##' @keywords data
##' @description Example foieGras mpm fit object. This example fit 
##' is included purely to speed up examples where a fit object is required
##' but fitting to data is not the focus of the example.
NULL

##' @name res
##' @docType data
##' @title foieGras example osar residuals object
##' @format .RData
##' @keywords data
##' @description Example foieGras osar residuals object. This example osar 
##' residuals object is included purely to speed up vignette build.
NULL

## stop R CMD check generating NOTES about global variables
globalVariables(c(".", "id", "tid", "ssm", "converged", "keep", "y", "x", "x.se", "y.se",
                  "geometry", "u", "v", "u.se", "v.se", "lc", "smaj", "smin", "eor",
                  "obs.type", "emf.x", "emf.y", "lon", "lat", "rename", "X", "Y", 
                  "y.z", "x.z",  "z", "out", "r", "sub", "isd", "digits", 
                  "lonerr", "laterr", "coord", "value", "resid", 
                  "se", "g", "g.se", "id1", "mpm", "residual",
                  "s", "s.se", "ci", "b", "x.err", "y.err", "xy", "ellps.tab"))