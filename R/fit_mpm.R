##' @title fit a a Move Persistence Model (mpm)
##' @description fit a random walk with time-varying move persistence to location data (e.g., output from \code{fit_ssm})
##' @param x a data frame of observations (see details)
##' @param model mpm model to fit; either \code{mpm} with unpooled random walk variance parameters (\code{sigma_(g,i)}) or \code{jmpm} with a single, pooled random variance parameter (\code{sigma_g})
##' @param optim numerical optimizer
##' @param verbose report progress during minimization
##' @param control list of control parameters for the outer optimization (type ?nlminb or ?optim for details)
##' @param inner.control list of control parameters for the inner optimization
##' @return a list with components
##' \item{\code{fitted}}{a dataframe of fitted locations}
##' \item{\code{par}}{model parameter summary}
##' \item{\code{data}}{input dataframe}
##' \item{\code{tmb}}{the tmb object}
##' \item{\code{opt}}{the object returned by the optimizer}
##' 
##' @examples
##' ## fit jmpm to two southern elephant seals
##' data(fssm)
##' dmp <- grab(fssm, "predicted", as_sf=FALSE)
##' dmp <- dmp[, c("id", "date", "lon", "lat")]
##' fmpm <- fit_mpm(dmp, model = "jmpm")
##' 
##' 
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom dplyr "%>%" group_by ungroup do mutate select
##' @export
fit_mpm <- function(x,
                    model = c("mpm", "jmpm"),
                    optim = c("nlminb", "optim"),
                    verbose = 1,
                    control = NULL,
                    inner.control = NULL) {
  
  optim <- match.arg(optim)
  model <- match.arg(model)
  
  if(verbose == 1)
    cat("\nfitting mpm...\n")
  if (verbose %in% 0:1)
    verb <-  FALSE
  else
    verb <- TRUE
  
  switch(model,
         mpm = {
           fit <- x %>%
             group_by(id) %>%
             do(mpm = try(mpmf(
               .,
               model = model,
               optim = optim,
               verbose = verb,
               control = control,
               inner.control = inner.control
             ),
             silent = TRUE)
             )
           
           fit <- fit %>%
             ungroup(.) %>%
             mutate(converged = sapply(.$mpm, function(x)
               if(length(x) == 8) {
                 x$opt$convergence == 0
               } else if(length(x) < 8) {
                 FALSE
               })) %>%
             select(., id, mpm, converged)
         },
         jmpm = {
           fit <- x %>%
             do(mpm = try(mpmf(
               .,
               model = model,
               optim = optim,
               verbose = verb,
               control = control,
               inner.control = inner.control
             ),
             silent = TRUE)
             )
           
           fit <- fit %>%
             mutate(converged = sapply(.$mpm, function(x)
               if(length(x) == 8) {
                 x$opt$convergence == 0
               } else if(length(x) < 8) {
                 FALSE
               })) %>%
             select(., mpm, converged)
         })

  class(fit) <- append("fG_mpm", class(fit))  

return(fit)
  
}
