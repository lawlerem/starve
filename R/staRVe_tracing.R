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

#' @export
#' @describeIn staRVe_tracing Get optimization time
setMethod(f = "opt_time",
          signature = "staRVe_tracing",
          definition = function(x) return(x@opt_time)
)
setReplaceMethod(f = "opt_time",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@opt_time<- value
  return(x)
})



#' @export
#' @describeIn staRVe_tracing Get hessian computation time
setMethod(f = "hess_time",
          signature = "staRVe_tracing",
          definition = function(x) return(x@hess_time)
)
#' @export
setReplaceMethod(f = "hess_time",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@hess_time<- value
  return(x)
})



#' @export
#' @describeIn staRVe_tracing Get standard error computation time
setMethod(f = "sdr_time",
          signature = "staRVe_tracing",
          definition = function(x) return(x@sdr_time)
)
setReplaceMethod(f = "sdr_time",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@sdr_time<- value
  return(x)
})



#' @export
#' @describeIn staRVe_tracing Get parameter hessian matrix
setMethod(f = "parameter_hessian",
          signature = "staRVe_tracing",
          definition = function(x) return(x@parameter_hessian)
)
setReplaceMethod(f = "parameter_hessian",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@parameter_hessian<- value
  return(x)
})



#' @export
#' @describeIn staRVe_tracing Get parameter covariance matrix
setMethod(f = "parameter_covariance",
          signature = "staRVe_tracing",
          definition = function(x) return(x@parameter_covariance)
)
setReplaceMethod(f = "parameter_covariance",
                 signature = "staRVe_tracing",
                 definition = function(x,value) {
  x@parameter_covariance<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @export
#' @describeIn staRVe_tracing Get all timing information
setMethod(f = "timing",
          signature = "staRVe_tracing",
          definition = function(x) {
  timings<- list(fit = opt_time(x),
                 hessian = hess_time(x),
                 sdr = sdr_time(x))
  return(timings)
})
