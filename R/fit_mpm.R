##' @title fit a a Move Persistence Model (mpm)
##' @description fit a random walk with time-varying move persistence to location data (e.g., output from \code{fit_ssm})
##' @param x a data frame of observations (see details)
##' @param model mpm model to fit; either \code{mpm} with unpooled random walk variance parameters (\code{sigma_(g,i)}) or \code{jmpm} with a single, pooled random variance parameter (\code{sigma_g})
##' @param optim numerical optimizer
##' @param optMeth optimization method to use (default is "L-BFGS-B"), ignored if optim = "nlminb" (see ?optim for details)
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
##' data(xs)
##' dmp <- grab(xs, "predicted", as_sf=FALSE)
##' dmp <- dmp[, c("id", "date", "lon", "lat")]
##' fmpm <- fit_mpm(dmp, model = "jmpm")
##' 
##' 
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom dplyr "%>%" mutate
##' @importFrom purrr map
##' @export
fit_mpm <- function(x,
                    model = c("mpm", "jmpm"),
                    optim = "optim",
                    optMeth = "L-BFGS-B",
                    verbose = 1,
                    control = NULL,
                    inner.control = NULL) {
  
  model <- match.arg(model)
  
  if(verbose == 1)
    cat(paste0("fitting ", model, "...\n"))

  switch(model,
         mpm = {
           fit <- split(x, x$id) %>%
             map(~ try(mpmf(.x, 
                        model = model,
                        optim = optim,
                        optMeth = optMeth,
                        verbose = verbose,
                        control = control,
                        inner.control = inner.control
             ), silent = TRUE)
             )
          fit <- tibble(id = names(fit), mpm = fit) %>%
            mutate(converged = sapply(.$mpm, function(x) 
              if(length(x) == 8) {
                x$opt$convergence == 0
              } else if(length(x) < 8) {
                FALSE
              })) %>%
            mutate(model = sapply(.$mpm, function(x) x$model))
         },
         jmpm = {
           fit <- try(mpmf( 
               x = x,
               model = model,
               optim = optim,
               optMeth = optMeth,
               verbose = verbose,
               control = control,
               inner.control = inner.control
             ), silent = TRUE)
           
           fit <- tibble(mpm = list(fit)) %>%
             mutate(converged = ifelse(length(.$mpm[[1]]) == 8, .$mpm[[1]]$opt$convergence == 0, FALSE)) %>%
             mutate(model = model)
         })

  class(fit) <- append("fG_mpm", class(fit))  

return(fit)
  
}
