#' @include classes.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' Create a \code{staRVe_tracing} object
#'
#' @export
setGeneric(name = "staRVe_tracing",
           def = function(x,...) standardGeneric("staRVe_tracing")
)
#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_tracing} instead.
#'
#' @export
#' @rdname staRVe_tracing
setMethod(
  f = "initialize",
  signature = "staRVe_tracing",
  definition = function(.Object,
                        opt_time = proc.time()-proc.time(),
                        hess_time = proc.time()-proc.time(),
                        sdr_time = proc.time()-proc.time(),
                        parameter_hessian = matrix(numeric(0),nrow=0,ncol=0),
                        parameter_covariance = matrix(numeric(0),nrow=0,ncol=0)) {
    opt_time(.Object)<- opt_time
    hess_time(.Object)<- hess_time
    sdr_time(.Object)<- sdr_time
    parameter_hessian(.Object)<- parameter_hessian
    parameter_covariance(.Object)<- parameter_covariance

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_tracing}.
#'
#' @param x An object of class \code{staRVe_tracing}.
#' @param value A replacement value
#'
#' @family Access_staRVe_tracing
#' @name Access_staRVe_tracing
NULL

#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "opt_time",
           def = function(x) standardGeneric("opt_time")
)
#' @export
setMethod(f = "opt_time",
          signature = "staRVe_tracing",
          definition = function(x) return(x@opt_time)
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "opt_time<-",
           def = function(x,value) standardGeneric("opt_time<-")
)
#' @export
setReplaceMethod(f = "opt_time",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@opt_time<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "hess_time",
           def = function(x) standardGeneric("hess_time")
)
#' @export
setMethod(f = "hess_time",
          signature = "staRVe_tracing",
          definition = function(x) return(x@hess_time)
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "hess_time<-",
           def = function(x,value) standardGeneric("hess_time<-")
)
#' @export
setReplaceMethod(f = "hess_time",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@hess_time<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "sdr_time",
           def = function(x) standardGeneric("sdr_time")
)
#' @export
setMethod(f = "sdr_time",
          signature = "staRVe_tracing",
          definition = function(x) return(x@sdr_time)
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "sdr_time<-",
           def = function(x,value) standardGeneric("sdr_time<-")
)
#' @export
setReplaceMethod(f = "sdr_time",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@sdr_time<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_hessian",
           def = function(x) standardGeneric("parameter_hessian")
)
#' @export
setMethod(f = "parameter_hessian",
          signature = "staRVe_tracing",
          definition = function(x) return(x@parameter_hessian)
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_hessian<-",
           def = function(x,value) standardGeneric("parameter_hessian<-")
)
#' @export
setReplaceMethod(f = "parameter_hessian",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@parameter_hessian<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_covariance",
           def = function(x) standardGeneric("parameter_covariance")
)
#' @export
setMethod(f = "parameter_covariance",
          signature = "staRVe_tracing",
          definition = function(x) return(x@parameter_covariance)
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_covariance<-",
           def = function(x,value) standardGeneric("parameter_covariance<-")
)
#' @export
setReplaceMethod(f = "parameter_covariance",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@parameter_covariance<- value
  return(x)
})
