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
##' @references Jonsen I, McMahon CR, Patterson TA, Auger-Méthé M, Harcourt R, Hindell MA, Bestley S (2019) 
##' Movement responses to environment: fast inference of variation among southern elephant seals with a mixed effects model. 
##' Ecology 100:e02566
##' @references Jonsen I, Patterson TA, Costa DP, Doherty PD, Godley BJ, Grecian WJ, Guinet C, Hoenner X, Kienle SS, 
##' Robinson PW, Votier SC, Witt MJ, Hindell MA, Harcourt RG, McMahon CR (2020) A continuous-time state-space model for rapid 
##' quality-control of Argos locations from animal-borne tags. arXiv doi:arxiv:2005.00401
##'
##' @keywords foieGras
##' @importFrom assertthat assert_that
##' @importFrom utils data flush.console
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
##' @importFrom stats approx cov sd predict nlminb optim na.omit median
##' @importFrom utils flush.console
##' @importFrom ggplot2 ggplot geom_point geom_path geom_ribbon geom_qq geom_qq_line geom_histogram aes ggtitle theme_bw
##' @importFrom ggplot2 theme element_blank geom_sf xlim ylim unit
##' @importFrom ggplot2 element_text scale_colour_viridis_c
##' @importFrom grDevices extendrange grey
NULL

##' @name ellie
##' @docType data
##' @title Southern elephant seal Argos satellite data (1 individual)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name ellies
##' @docType data
##' @title Southern elephant seal Argos satellite data (2 individuals)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name fssm
##' @docType data
##' @title foieGras example fit object
##' @format .RData
##' @keywords data
##' @description Example foieGras fit object, using ellies example data and the
##' following call: fssm <- fit_ssm(ellies, vmax=4, model="crw", time.step=36). This example
##' fit is included purely to speed up examples where a fit object is required
##' but fitting to data is not the focus of the example.
NULL

##' @name fmpm
##' @docType data
##' @title foieGras example mpm fit object
##' @format .RData
##' @keywords data
##' @description Example foieGras fit object, using ssm_fits example data and the
##' following call: fmp <- fssm %>% grab(., "p", as_sf = FALSE) %>% 
##' select(id,date,lon,lat) %>% 
##' fit_mpm(., model="jmpm") 
##' This example fit is included purely to speed up examples where a fit object is required
##' but fitting to data is not the focus of the example.
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## stop R CMD check generating NOTES about global variables
id <- tid <- ssm <- converged <- keep <- id <- y <- x <- x.se <- y.se <- "shut.up"
geometry <- u <- v <- u.se <- v.se <- lc <- smaj <- smin <- eor <- "shut.up"
obs.type <- emf.x <- emf.y <- lon <- lat <- rename <- X <- Y <- "shut.up"
y.z <- x.z <- z <- out <- r <- sub <- isd <- digits <- "shut.up"
lonerr <- laterr <- coord <- value <- resid <- "shut.up"
se <- g <- g.se <- id1 <- mpm <- residual <- "shut.up"