#' @include classes.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' Create a \code{staRVe_observation_parameters} object
#'
#' @export
setGeneric(name = "staRVe_observation_parameters",
           def = function(x,...) standardGeneric("staRVe_observation_parameters")
)
#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_observation_parameters} instead.
#'
#' @export
#' @rdname staRVe_observation_parameters
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
#' @family Access_staRVe_observation_parameters
#' @name Access_staRVe_observation_parameters
NULL

#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_distribution",
           def = function(x) standardGeneric("response_distribution")
)
#' @export
setMethod(f = "response_distribution",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_distribution)
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_distribution<-",
           def = function(x,value) standardGeneric("response_distribution<-")
)
#' @export
setReplaceMethod(f = "response_distribution",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_distribution<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_parameters",
           def = function(x) standardGeneric("response_parameters")
)
#' @export
setMethod(f = "response_parameters",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_parameters)
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_parameters<-",
           def = function(x,value) standardGeneric("response_parameters<-")
)
#' @export
setReplaceMethod(f = "response_parameters",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_parameters<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "link_function",
           def = function(x) standardGeneric("link_function")
)
#' @export
setMethod(f = "link_function",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@link_function)
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "link_function<-",
           def = function(x,value) standardGeneric("link_function<-")
)
#' @export
setReplaceMethod(f = "link_function",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@link_function<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "fixed_effects",
           def = function(x) standardGeneric("fixed_effects")
)
#' @export
setMethod(f = "fixed_effects",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@fixed_effects)
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "fixed_effects<-",
           def = function(x,value) standardGeneric("fixed_effects<-")
)
#' @export
setReplaceMethod(f = "fixed_effects",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@fixed_effects<- value
  return(x)
})
