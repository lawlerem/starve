#' @include classes.R generics.R staRVe_process.R staRVe_observations.R staRVe_settings.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_model} instead.
#'
#' @export
#' @rdname staRVe_model
setMethod(
  f = "initialize",
  signature = "staRVe_model",
  definition = function(.Object,
                        process = new("staRVe_process"),
                        observations = new("staRVe_observations"),
                        settings = new("staRVe_settings")) {
    process(.Object)<- process
    observation(.Object)<- observations
    settings(.Object)<- settings

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_model}.
#'
#' @param x An object of class \code{staRVe_model}.
#' @param value A replacement value
#'
#' @family Access_staRVe_model
#' @name Access_staRVe_model
NULL

#' @export
setMethod(f = "process",
          signature = "staRVe_model",
          definition = function(x) return(x@process)
)
#' @export
setReplaceMethod(f = "process",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@process<- value
  return(x)
})



#' @export
setMethod(f = "observations",
          signature = "staRVe_model",
          definition = function(x) return(x@observations)
)
#' @export
setReplaceMethod(f = "observations",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@observations<- value
  return(x)
})



#' @export
setMethod(f = "settings",
          signature = "staRVe_model",
          definition = function(x) return(x@settings)
)
#' @export
setReplaceMethod(f = "settings",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@settings<- value
  return(x)
})
