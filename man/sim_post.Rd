% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_post.R
\name{sim_post}
\alias{sim_post}
\title{simulate from the posterior of a \code{ssm} fit.}
\usage{
sim_post(x, what = "predicted", reps = 1, sim_only = FALSE)
}
\arguments{
\item{x}{a \code{ssm} fit object with class \code{ssm_df}}

\item{what}{simulate fitted or predicted locations}

\item{reps}{number of replicate tracks to simulate from the \code{ssm} model
fit object}

\item{sim_only}{logical, do not include \code{ssm} estimated locations in
output (default is FALSE)}
}
\value{
a \code{fG_sim_post} object containing the paths simulated from a
\code{ssm} fit object
}
\description{
simulates track locations from the joint precision matrix of a
\code{ssm} model fit. Currently, the joint precision of the SSM movement
parameters is not included (ie. a full posterior simulation).
}
\examples{
fit <- fit_ssm(ellie, model = "crw", time.step = 24)
psim <- sim_post(fit, "p", reps = 10)
plot(psim, type = "lines")

}
