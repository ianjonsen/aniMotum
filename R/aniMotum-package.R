##' \pkg{aniMotum}
##'
##' fit Continuous-Time Random Walk and Correlated Random Walk state-space models to filter Argos Least Squares or Kalman Filter location data
##'
##' @name aniMotum-package
##' @aliases aniMotum aniMotum-package
##' @docType package
##' @author
##' **Maintainer:** Ian Jonsen <ian.jonsen@mq.edu.au> [ORCID](https://orcid.org/0000-0001-5423-6076)  
##' 
##' Toby Patterson [ORCID](https://orcid.org/0000-0002-7150-9205)  
##' 
##' W. James Grecian [ORCID](https://orcid.org/0000-0002-6428-719X)  
##'
##' @seealso Useful Links:
##' * [https://ianjonsen.github.io/aniMotum/](https://ianjonsen.github.io/aniMotum/)
##' * [https://github.com/ianjonsen/aniMotum/](https://github.com/ianjonsen/aniMotum/)
##' * Report bugs/issues at [https://github.com/ianjonsen/aniMotum/issues/](https://github.com/ianjonsen/aniMotum/issues/)
##' 
##' @references Jonsen ID, Grecian WJ, Phillips L, et al. (2023) `aniMotum`, an R package for animal movement data: rapid quality control, behavioural estimation and simulation. Methods in Ecology and Evolution. Accepted 06/12/2022.
##' @references Jonsen ID, Patterson TA, Costa DP, et al. (2020) A continuous-time state-space model for rapid quality-control of Argos locations from animal-borne tags. Movement Ecology 8:31.
##' @references Jonsen ID, McMahon CR, Patterson TA, et al. (2019) Movement responses to environment: fast inference of variation among southern elephant seals with a mixed effects model. Ecology. 100(1):e02566.
##' 
##' @keywords aniMotum
##' @importFrom utils data flush.console globalVariables
##' @importFrom dplyr select mutate slice group_by
##' @importFrom dplyr distinct arrange left_join lag full_join bind_cols
##' @importFrom dplyr summarise
##' @importFrom tibble as_tibble tibble
##' @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##' @importFrom sf st_coordinates st_geometry<- st_bbox st_cast
##' @importFrom terra ext extract
##' @importFrom traipse track_distance track_angle track_distance_to
##' @importFrom TMB MakeADFun sdreport newtonOption oneStepPredict
##' @importFrom stats approx cov sd predict nlminb optim na.omit median qlogis qnorm pnorm runif
##' @importFrom utils flush.console globalVariables
##' @importFrom ggplot2 ggplot geom_point geom_path geom_ribbon geom_qq geom_qq_line geom_histogram aes ggtitle theme_bw
##' @importFrom ggplot2 theme element_blank geom_sf xlim ylim unit aes_string
##' @importFrom ggplot2 element_text scale_colour_manual scale_colour_gradientn
##' @importFrom grDevices extendrange grey hcl.colors
##' @importFrom rnaturalearth ne_countries
NULL

##' @name ellie
##' @docType data
##' @title Southern elephant seal Argos satellite data (1 individual, sub-sampled for testing speed)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) Sourced 
##' from the Australian Integrated Marine Observing System (IMOS) deployments at 
##' Davis Research Station, Antarctica and are publicly available (http:// imos.aodn.org.au). 
##' IMOS is supported by the Australian Government through the National Collaborative 
##' Research Infrastructure Strategy and the Super Science Initiative. 
NULL

##' @name sese
##' @docType data
##' @title Southern elephant seal Argos satellite data (3 individuals)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) Sourced 
##' from the Australian Integrated Marine Observing System (IMOS) in collaboration
##' with the French IPEV and SNO-MEMO project deployments at Iles Kerguelen and 
##' are publicly available (http:// imos.aodn.org.au). IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative. 
NULL

##' @name sese2
##' @docType data
##' @title Southern elephant seal Argos satellite data (2 highly sub-sampled individuals)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data, highly sub-sampled. 
##' These example data are included purely to speed up examples where a fit 
##' object is required. Generating a fit object is preferred as storing an 
##' example fit risks GDAL errors on platforms with older GDAL libraries. Sourced 
##' from the Australian Integrated Marine Observing System (IMOS) in collaboration
##' with the French IPEV and SNO-MEMO project deployments at Iles Kerguelen and 
##' are publicly available (http:// imos.aodn.org.au). IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative. 
NULL

##' @name wese_sb
##' @docType data
##' @title Weddell seal Argos satellite data (1 individual)
##' @format .RData
##' @keywords data
##' @description Example Weddell seal Argos tracking data, deployed at Scott Base,
##' Ross Island, Antarctica. This example data set is included for demonstration
##' purposes. Sourced from the Australian Integrated Marine Observing System (IMOS) 
##' & the New Zealand National Institute of Water and Atmospheric Research (NIWA)
##' deployments at Scott Base, Antarctica and are publicly available (http:// imos.aodn.org.au).
##' IMOS is supported by the Australian Government through the National Collaborative 
##' Research Infrastructure Strategy and the Super Science Initiative. 
NULL

##' @name sese2_n
##' @docType data
##' @title Southern elephant seal Argos satellite data (2 highly sub-sampled individuals)
##' @format .RData
##' @keywords data
##' @description Example elephant seal Argos tracking data, highly sub-sampled with 
##' default variable order scrambled and renamed. These example data are included purely to 
##' speed up examples where a fit object is required. Generating a fit object is 
##' preferred as storing an example fit risks GDAL errors on platforms with older 
##' GDAL libraries. Sourced from the Australian Integrated Marine Observing System (IMOS) 
##' in collaboration with the French IPEV and SNO-MEMO project deployments at 
##' Iles Kerguelen and are publicly available (http:// imos.aodn.org.au). IMOS 
##' is supported by the Australian Government through the National Collaborative 
##' Research Infrastructure Strategy and the Super Science Initiative. 
NULL

## stop R CMD check generating NOTES about global variables
globalVariables(c(".", "id", "tid", "ssm", "converged", "keep", "y", "x", "x.se", "y.se",
                  "geometry", "u", "v", "u.se", "v.se", "lc", "smaj", "smin", "eor",
                  "obs.type", "emf.x", "emf.y", "lon", "lat", "rename", "X", "Y", 
                  "y.z", "x.z",  "z", "out", "r", "sub", "isd", "digits", "map",
                  "lonerr", "laterr", "coord", "value", "resid", "long", "cluster",
                  "se", "g", "logit_g", "logit_g.se", "id1", "mpm", "residual", "group", "availableCores",
                  "s", "s.se", "ci", "b", "x.err", "y.err", "xy", "ellps.tab", "sims", "flg",
                  "pts", "rrt_pts", "pts_rrt", "pts_fix", "model", "dist", "bear"))