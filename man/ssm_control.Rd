% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssm_control.R
\name{ssm_control}
\alias{ssm_control}
\title{Control Values for \code{fit_ssm}.}
\usage{
ssm_control(
  optim = c("nlminb", "optim"),
  method = c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent"),
  lower = NULL,
  upper = NULL,
  verbose = 1,
  se = FALSE,
  ...
)
}
\arguments{
\item{optim}{the numerical optimizer used in the fit}

\item{method}{if optim = "optim" then the optimization method to be used
can be one of "BFGS", "L-BFGS-B", "Nelder-Mead", "CG", "SANN", or "Brent"
see \code{\link{optim}} for details}

\item{lower}{a list named parameter lower bounds, if NULL then built in
defaults are used when \code{method = "L-BFGS-B"}. Possible parameter names are:
\code{l_sigma} a vector of length 2, log scale; \code{l_rho_p} a scalar, logit scale;
\code{l_D} a scalar, log scale; \code{l_psi} a scalar, log scale;
\code{l_tau} a vector of length 2, log scale; \code{l_rho_o} a scalar, logit scale}

\item{upper}{a list of named parameter upper bounds, if NULL then built in
defaults are used when \code{method = "L-BFGS-B"}. Possible parameter names are same as \code{lower}}

\item{verbose}{integer; report progress during minimization: 0 = silent;
1 = parameter trace (default); 2 = optimizer trace}

\item{se}{logical; should standard errors for speed estimates be calculated (default = FALSE).
Turning this on will slow down computation time but provide SE's for speed-along-track calculations}

\item{...}{control parameters for the chosen optimizer}
}
\value{
Returns a list with components
\item{\code{optim}}{the name of the numerical optimizer as a
string, "nlminb" or "optim"}
\item{\code{method}}{optimization method to be used}
\item{\code{lower}}{named list of lower parameter bounds}
\item{\code{upper}}{named list of upper parameter bounds}
\item{\code{verbose}}{level of tracing information to be reported}
\item{\code{control}}{list of control parameters for the optimizer}
}
\description{
\code{ssm_control} selects the numerical minimizer, method, associated
control parameters, and parameter bounds used by \code{fit_ssm}.
}
\details{
The optimizer used to minimize the objective function is
selected by the \code{optim} argument.  Additional control
parameters specific to the chosen optimizer are specified via the
dots argument.  See \code{\link{nlminb}} and \code{\link{optim}}
for available options. Adapted from S. Wotherspoon
\url{https://github.com/SWotherspoon/RWalc/blob/master/R/RWalc.R}
}
\examples{
fit <- fit_ssm(ellie,
vmax = 4,
model = "crw",
time.step = 72,
control = ssm_control(
    optim = "nlminb",
    eval.max = 2000)
    )
}
\seealso{
\code{\link{nlminb}}, \code{\link{optim}}.
}
