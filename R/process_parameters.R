#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param covariance_function Which covariance function to use
#' @param time_parameters A data.frame
#'
#' @noRd
setMethod(
  f = "initialize",
  signature = "process_parameters",
  definition = function(.Object,
                        covariance_function = "exponential",
                        time_parameters = list(data.frame(par = c(0,0,0),
                                                          se = c(0,0,0),
                                                          fixed = c(FALSE,FALSE,FALSE),
                                                          row.names = c("mu","ar1","sd")))) {
    covariance_function(.Object)<- covariance_function
    # covariance function takes care of spatial parameters
    time_parameters(.Object)<- time_parameters

    return(.Object)
  }
)


##############
###        ###
### Access ###
###        ###
##############

#' @param x An object
#' @describeIn process_parameters_class Get covariance function(s)
setMethod(f = "covariance_function",
          signature = "process_parameters",
          definition = function(x) return(x@covariance_function)
)
#' @param x An object
#' @param value A replacement value
#' @describeIn process_parameters_class Set covariance function(s). Run
#'   get_starve_distributions("covariance") for valid covariance functions.
#'   Setting the covariance function also overwrites the spatial parameters.
setReplaceMethod(f = "covariance_function",
                 signature = "process_parameters",
                 definition = function(x,value) {
  value<- unname(get_starve_distributions("covariance")[pmatch(value,get_starve_distributions("covariance"),duplicates.ok=TRUE)])
  x@covariance_function<- value
  spatial_pars<- lapply(value,function(cf) {
    switch(cf,
      exponential = data.frame(par = c(0,0,0.5),
                               se = c(0,0,0),
                               fixed = c(FALSE,FALSE,TRUE),
                               row.names = c("sd","range","nu")),
      gaussian = data.frame(par = c(0,0,Inf),
                               se = c(0,0,0),
                               fixed = c(FALSE,FALSE,TRUE),
                               row.names = c("sd","range","nu")),
      matern = data.frame(par = c(0,0,0.5),
                          se = c(0,0,0),
                          fixed = c(FALSE,FALSE,FALSE),
                          row.names = c("sd","range","nu")),
      matern32 = data.frame(par = c(0,0,1.5),
                            se = c(0,0,0),
                            fixed = c(FALSE,FALSE,TRUE),
                            row.names = c("sd","range","nu")),
    )
  })
  try(names(spatial_pars)<- names(spatial_parameters(x)))

  spatial_parameters(x)<- spatial_pars
  return(x)
})


#' @param x An object
#' @describeIn process_parameters_class Get spatial parameters
setMethod(f = "spatial_parameters",
          signature = "process_parameters",
          definition = function(x) return(x@spatial_parameters)
)
#' @param x An object
#' @param value A replacement value
#' @describeIn process_parameters_class Set spatial parameters
setReplaceMethod(f = "spatial_parameters",
                 signature = "process_parameters",
                 definition = function(x,value) {
  x@spatial_parameters<- value
  return(x)
})


#' @param x An object
#' @describeIn process_parameters_class Get time parameters
setMethod(f = "time_parameters",
          signature = "process_parameters",
          definition = function(x) return(x@time_parameters)
)
#' @param x An object
#' @param value A replacement value
#' @describeIn process_parameters_class Set time parameters
setReplaceMethod(f = "time_parameters",
                 signature = "process_parameters",
                 definition = function(x,value) {
  x@time_parameters<- value
  return(x)
})
