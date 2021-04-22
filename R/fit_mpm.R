##' @title fit a a Move Persistence Model (mpm)
##' @description fit a random walk with time-varying move persistence to temporally regular or irregular location data
##' @param x a `fG_ssm` fit object or a data frame of observations (see details)
##' @param what if a `fG_ssm` fit object is supplied then \code{what} determines whether fitted or predicted (default) values are mapped; ignored if \code{x} is a data frame
##' @param model mpm model to fit; either \code{mpm} with unpooled random walk variance parameters (\code{sigma_(g,i)}) or \code{jmpm} with a single, pooled random variance parameter (\code{sigma_g})
##' @param coords column numbers of the location coordinates (default = 3:4)
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
##' ## fit jmpm to two southern elephant seal tracks
##' data(xs)
##' fmpm <- fit_mpm(xs, model = "jmpm")
##' 
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom dplyr "%>%" mutate select
##' @importFrom purrr map
##' @export
fit_mpm <- function(x,
                    what = "predicted",
                    model = c("jmpm", "mpm"),
                    coords = 3:4,
                    optim = "optim",
                    optMeth = "L-BFGS-B",
                    verbose = 1,
                    control = NULL,
                    inner.control = NULL) {
  
  model <- match.arg(model)
  
  if(verbose == 1)
    cat(paste0("fitting ", model, "...\n"))

  if(inherits(x, "fG_ssm")) {
    x <- grab(x, what = what, as_sf = FALSE) %>%
      select(id, date, coords[1], coords[2])
  } else {
    x <- x %>% select(id, date, coords[1], coords[2])
  }
  
  if(all(c("x","y") %in% names(x))) {
    # rescale x,y in km for better optimisation
    xm <- max(x$x)
    ym <- max(x$y)
    x <- x %>% mutate(x = x/xm, y = y/ym)
  } else {
    # standardise coord names to x,y 
    names(x)[3:4] <- c("x","y")
  }

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
              }))
          browser()
          fit <- tibble(fit, model)
          fit$model[which(fit$converged)] <- sapply(fit[which(fit$converged), ]$mpm, function(x) x$model)
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
