#' @include classes.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_observation_parameters} instead.
#'
#' @export
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

#' Get or set slots from an object of class \code{staRVe_observation_parameters}.
#'
#' @param x An object of class \code{staRVe_observation_parameters}.
#' @param value A replacement value
#'
#' @family access_staRVe_observation_parameters
#' @name access_staRVe_observation_parameters
NULL

#' @export
#' @rdname access_staRVe_observation_parameters
setMethod(f = "response_distribution",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_distribution)
)
#' @export
#' @rdname access_staRVe_observation_parameters
setReplaceMethod(f = "response_distribution",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_distribution<- value
  return(x)
})



#' @export
#' @rdname access_staRVe_observation_parameters
setMethod(f = "response_parameters",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_parameters)
)
#' @export
#' @rdname access_staRVe_observation_parameters
setReplaceMethod(f = "response_parameters",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_parameters<- value
  return(x)
})



#' @export
#' @rdname access_staRVe_observation_parameters
setMethod(f = "link_function",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@link_function)
)
#' @export
#' @rdname access_staRVe_observation_parameters
setReplaceMethod(f = "link_function",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@link_function<- value
  return(x)
})



#' @export
#' @rdname access_staRVe_observation_parameters
setMethod(f = "fixed_effects",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@fixed_effects)
)
#' @export
#' @rdname access_staRVe_observation_parameters
setReplaceMethod(f = "fixed_effects",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@fixed_effects<- value
  return(x)
})
