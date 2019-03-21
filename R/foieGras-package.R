##' \pkg{foieGras}
##'
##' fit Continuous-Time Random Walk and Correlated Random Walk state-space models to filter Argos Least Squares or Kalman Filter location data
##'
##' @name foieGras-package
##' @aliases foieGras foieGras-package
##' @docType package
##' @author Ian Jonsen, Toby Patterson
##'
##'  Maintainer: Ian Jonsen <ian.jonsen@mq.edu.au>
##'  @seealso fit_ssm
##'  @references Jonsen I, McMahon CR, Patterson TA, Auger-Methe M, Harcourt R, Hindell MA, Bestley S (2019) Movement responses to environment: fast inference of variation among southern elephant seals with a mixed effects model. Ecology 100:e02566
##'
##'  @keywords foieGras
##'  @importFrom utils data
##'  @importFrom dplyr group_by do rowwise %>% ungroup select mutate slice
##'  @importFrom dplyr distinct arrange filter left_join lag full_join bind_cols
##'  @importFrom dplyr summarise
##'  @importFrom tibble as_tibble
##'  @importFrom lubridate ymd_hms
##'  @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##'  @importFrom sf st_coordinates st_geometry<- st_bbox st_cast
##'  @importFrom argosfilter sdafilter
##'  @importFrom TMB MakeADFun sdreport newtonOption
##'  @importFrom stats approx cov sd predict nlminb optim na.omit
##'  @importFrom ggplot2 ggplot geom_point geom_path aes ggtitle theme_bw
##'  @importFrom ggplot2 theme element_blank geom_sf xlim ylim unit
##'  @importFrom ggplot2 element_text scale_colour_viridis_c
##'  @importFrom gridExtra grid.arrange
##'  @importFrom grDevices extendrange grey
NULL

##' @name ellie
##' @docType data
##' @title Elephant seal Argos satellite data (1 individual)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name rope
##' @docType data
##' @title Royal penguin Argos satellite data (13 individuals)
##' @format .RData
##' @keywords data
##' @description Example penguin Argos tracking data.
NULL

##' @name testfit
##' @docType data
##' @title a test foieGras fitted model object
##' @format .RData
##' @keywords data
##' @description a foieGras fitted model object, using the ellie example data,
##' for package testing purposes
NULL
