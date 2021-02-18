#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @noRd
setMethod(
  f = "initialize",
  signature = "staRVe_observation_parameters",
  definition = function(.Object,
                        response_distribution = character(0),
                        response_parameters = data.frame(par = numeric(0),
                                                         fixed = numeric(0)),
                        link_function = character(0),
                        fixed_effects = data.frame(par = numeric(0),
                                                   fixed = numeric(0))) {
    response_distribution(.Object)<- response_distribution
    response_parameters(.Object)<- response_parameters
    link_function(.Object)<- link_function
    fixed_effects(.Object)<- fixed_effects

    return(.Object)
  }
)


##############
###        ###
### Access ###
###        ###
##############

#' @export
#' @describeIn staRVe_observation_parameters Get/set response distribution. Run
#'   get_staRVe_distributions("distribution") for valid options.
setMethod(f = "response_distribution",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_distribution)
)
#' @export
setReplaceMethod(f = "response_distribution",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_distribution<- value
  return(x)
})



#' @export
#' @describeIn staRVe_observation_parameters Get/set response distribution parameters
setMethod(f = "response_parameters",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_parameters)
)
#' @export
setReplaceMethod(f = "response_parameters",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_parameters<- value
  return(x)
})



#' @export
#' @describeIn staRVe_observation_parameters Get/set link function.  Run
#'   get_staRVe_distributions("link") for valid options.
setMethod(f = "link_function",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@link_function)
)
#' @export
setReplaceMethod(f = "link_function",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@link_function<- value
  return(x)
})



#' @export
#' @describeIn staRVe_observation_parameters Get/set fixed effects
setMethod(f = "fixed_effects",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@fixed_effects)
)
#' @export
setReplaceMethod(f = "fixed_effects",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@fixed_effects<- value
  return(x)
})
