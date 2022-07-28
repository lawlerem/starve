#' @include classes.R getset.R generics.R staRVe_tracing.R staRVe_model.R TMB_out.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param starve A starve object
#' @param tracing A tracing object
#' @param TMB_out A TMB_out object
#'
#' @rdname starve-construct
setMethod(
  f = "initialize",
  signature = "starve_fit",
  definition = function(.Object,
                        starve = new("starve"),
                        tracing = new("tracing"),
                        TMB_out = new("TMB_out")) {
    as(.Object,"starve")<- starve
    tracing(.Object)<- tracing
    TMB_out(.Object)<- TMB_out

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' @param x An object
#'
#' @export
#' @describeIn starve_fit_class Get tracing information, see \link{tracing_class}.
setMethod(f = "tracing",
          signature = "starve_fit",
          definition = function(x) return(x@tracing)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn starve_fit_class Set tracing information (for internal use only)
setReplaceMethod(f = "tracing",
                 signature = "starve_fit",
                 definition = function(x,value) {
  x@tracing<- value
  return(x)
})


#' @param x An object
#'
#' @describeIn starve_fit_class Get TMB objects (for internal use only)
setMethod(f = "TMB_out",
          signature = "starve_fit",
          definition = function(x) return(x@TMB_out)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn starve_fit_class Set TMB objects (for internal use only)
setReplaceMethod(f = "TMB_out",
                 signature = "starve_fit",
                 definition = function(x,value) {
  x@TMB_out<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @param x An object
#'
#' @export
#' @describeIn starve_fit_class Get convergence message from optimizer
setMethod(f = "convergence",
          signature = "starve_fit",
          definition = function(x) {
  return(convergence(TMB_out(x)))
})

#' @param x An object
#'
#' @export
#' @describeIn starve_fit_class Get all timing information, see \link{tracing}
setMethod(f = "timing",
          signature = "starve_fit",
          definition = function(x) {
  return(timing(tracing(x)))
})
