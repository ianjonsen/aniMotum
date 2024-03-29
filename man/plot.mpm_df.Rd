% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.mpm.R
\name{plot.mpm_df}
\alias{plot.mpm_df}
\title{plot}
\usage{
\method{plot}{mpm_df}(
  x,
  y = NULL,
  se = FALSE,
  pages = 0,
  ncol = 1,
  ask = TRUE,
  pal = "Plasma",
  rev = FALSE,
  ...
)
}
\arguments{
\item{x}{a \code{aniMotum} \code{mpm} fit object with class \code{mpm_df}}

\item{y}{optional \code{ssm} fit object with class \code{ssm_df} corresponding to x.
If absent, 1-d plots of \code{gamma_t} time series are rendered
otherwise, 2-d track plots with locations coloured by `gamma_t`` are rendered.}

\item{se}{logical (default = FALSE); should points be scaled by \code{gamma_t}
uncertainty (ignored if y is not supplied)}

\item{pages}{plots of all individuals on a single page (pages = 1; default)
or each individual on a separate page (pages = 0)}

\item{ncol}{number of columns to use for faceting. Default is ncol = 1 but
this may be increased for multi-individual objects. Ignored if pages = 0}

\item{ask}{logical; if TRUE (default) user is asked for input before each
plot is rendered. set to FALSE to return ggplot objects}

\item{pal}{\link[grDevices:palettes]{grDevices::hcl.colors} palette to use (default: "Plasma";
see \link[grDevices:palettes]{grDevices::hcl.pals} for options)}

\item{rev}{reverse colour palette (logical)}

\item{...}{additional arguments to be ignored}
}
\value{
a ggplot object with either: 1-d time series of \code{gamma_t} estimates
(if y not provided), with estimation uncertainty ribbons (95 \% CI's);
or 2-d track plots (if y provided) coloured by \code{gamma_t}, with smaller points
having greater uncertainty (size is proportional to \code{SE^-2}, if \code{se = TRUE}).
Plots can be rendered all on a single page (pages = 1) or on separate pages.
}
\description{
visualize fits from an mpm object
}
\examples{

# generate a ssm fit object (call is for speed only)
xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, control = ssm_control(verbose = 0))

# fit mpm to ssm fits
xm <- fit_mpm(xs, model = "jmpm")

# plot 1-D mp timeseries on 1 page
plot(xm, pages = 1)


}
