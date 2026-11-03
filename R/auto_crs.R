##' @title Choose a projection from the extent of the data
##'
##' @description Selects a coordinate reference system suited to the geographic
##' extent of a set of tracks, for use by [aniMotum::fit_ssm] when
##' `projection = "auto"`.
##'
##' @details State-space model parameters are estimated in map coordinates, so
##' the projection is part of the model. The default global Mercator grid
##' stretches distances by `1 / cos(latitude)`, which is harmless for tracks
##' confined to a narrow band of latitude and substantial for tracks that are
##' not: see `vignette("Projections", package = "aniMotum")`.
##'
##' Two properties matter for a movement model, and they pull in different
##' directions.
##'
##' \strong{Local isotropy} - that a step of a given length on the ground maps
##' to the same coordinate length whichever way it points. Projections with
##' this property are conformal. Without it, the x and y process variances
##' differ for reasons that have nothing to do with the animal, and the
##' estimated direction of a step is wrong, which reaches `rho_p` and the move
##' persistence `g`. Every projection chosen here is conformal.
##'
##' \strong{Slowly varying scale} across the extent of the data, so that a
##' single `sigma` or `D` is not averaging over places that are stretched by
##' different amounts. No projection of a sphere achieves this exactly; the
##' choice is which error to make small.
##'
##' The rules, applied to the extent of \emph{all} the supplied locations:
##' \describe{
##'   \item{Mercator}{kept when the stretch varies by less than `thresh` across
##'   the data and the extremes are below 60 degrees. This preserves existing
##'   behaviour wherever it was never a problem.}
##'   \item{Lambert conformal conic}{for data within one hemisphere and below
##'   80 degrees. Standard parallels are placed one sixth and five sixths of
##'   the way through the latitude range, which is the usual rule for
##'   minimising scale error across a band. Scale depends only on latitude, so
##'   a wide longitude span costs nothing.}
##'   \item{Polar stereographic}{for data reaching beyond 80 degrees, or
##'   circumpolar data (more than 180 degrees of longitude above 55 degrees),
##'   where a conic's central meridian stops being meaningful.}
##'   \item{Oblique stereographic}{centred on the data when they straddle the
##'   equator, since a conic needs both standard parallels in one hemisphere.}
##' }
##'
##' The choice is made once from all individuals together, never per
##' individual. Fitting each animal in its own projection would make their
##' parameter estimates incomparable, which defeats the purpose of fitting a
##' deployment - and for a joint model it would make pooling meaningless.
##'
##' @param lon vector of longitudes, in degrees
##' @param lat vector of latitudes, in degrees
##' @param units distance units for the returned CRS. Default `"km"`, which
##' aniMotum uses internally because it helps the optimiser converge
##' @param thresh the Mercator stretch ratio above which a different
##' projection is chosen. Default 1.2, ie. a 20 percent difference in stretch
##' between the least and most distorted parts of the data
##'
##' @return a proj4 string
##'
##' @examples
##' ## four elephant seals from Iles Kerguelen, spanning 47 to 69 degrees south
##' auto_crs(sese$lon, sese$lat)
##'
##' ## a track confined to temperate latitudes keeps the Mercator default
##' auto_crs(c(150, 152), c(-34, -35))
##'
##' @export

auto_crs <- function(lon, lat, units = "km", thresh = 1.2) {

  ok <- is.finite(lon) & is.finite(lat)
  lon <- lon[ok]
  lat <- lat[ok]

  if (!length(lat))
    stop("no finite locations from which to choose a projection", call. = FALSE)

  d2r <- pi / 180
  lat.rng <- range(lat)
  abs.rng <- range(abs(lat))
  span <- diff(lat.rng)

  ## circular mean longitude, so that data straddling the antimeridian get a
  ## sensible central meridian rather than one on the far side of the world
  lon0 <- atan2(mean(sin(lon * d2r)), mean(cos(lon * d2r))) / d2r
  lon0 <- round(lon0)
  lon.span <- diff(range(wrap_lon(lon, lon0 - 180)))

  straddles.eq <- lat.rng[1] < 0 && lat.rng[2] > 0
  polar <- abs.rng[2] >= 80
  circumpolar <- lon.span > 180 && abs.rng[1] > 55

  ## how much does the Mercator stretch vary across these data?
  stretch <- 1 / cos(abs.rng * d2r)
  ratio <- stretch[2] / stretch[1]

  ## keep the existing default wherever it was never doing harm
  if (ratio < thresh && abs.rng[2] < 60)
    return(paste0("+proj=merc +lon_0=", lon0,
                  " +datum=WGS84 +units=", units, " +no_defs"))

  if (polar || circumpolar) {
    ## polar stereographic at the pole of the occupied hemisphere
    lat0 <- if (mean(lat) < 0) -90 else 90
    lat.ts <- round(mean(abs.rng)) * sign(lat0)
    return(paste0("+proj=stere +lat_0=", lat0, " +lat_ts=", lat.ts,
                  " +lon_0=", lon0,
                  " +datum=WGS84 +units=", units, " +no_defs"))
  }

  if (straddles.eq) {
    ## a conic needs both standard parallels in one hemisphere, so fall back to
    ## an oblique stereographic centred on the data
    lat0 <- round(mean(lat.rng))
    return(paste0("+proj=stere +lat_0=", lat0, " +lon_0=", lon0,
                  " +datum=WGS84 +units=", units, " +no_defs"))
  }

  ## Lambert conformal conic, standard parallels one sixth and five sixths of
  ## the way through the latitude range
  lat1 <- lat.rng[1] + span / 6
  lat2 <- lat.rng[2] - span / 6

  ## a degenerate cone if the band is very narrow; nudge the parallels apart
  if (abs(lat2 - lat1) < 0.5) {
    mid <- mean(lat.rng)
    lat1 <- mid - 0.25
    lat2 <- mid + 0.25
  }

  paste0("+proj=lcc +lat_1=", round(lat1, 2), " +lat_2=", round(lat2, 2),
         " +lat_0=", round(mean(lat.rng), 2), " +lon_0=", lon0,
         " +datum=WGS84 +units=", units, " +no_defs")
}
