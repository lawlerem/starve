#' @include classes.R generics.R staRVe_tracing.R staRVe_model.R TMB_out.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_fit} instead.
#'
#' @export
#' @rdname staRVe_fit
setMethod(
  f = "initialize",
  signature = "staRVe_fit",
  definition = function(.Object,
                        staRVe_model = new("staRVe_model"),
                        tracing = new("staRVe_tracing"),
                        TMB_out = new("TMB_out")) {
    as(.Object,"staRVe_model")<- staRVe_model
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

#' Get or set slots from an object of class \code{staRVe_fit}.
#'
#' @param x An object of class \code{staRVe_fit}.
#' @param value A replacement value
#'
#' @family Access_staRVe_fit
#' @name Access_staRVe_fit
NULL

#' @export
setMethod(f = "tracing",
          signature = "staRVe_fit",
          definition = function(x) return(x@tracing)
)
#' @export
setReplaceMethod(f = "tracing",
                 signature = "staRVe_fit",
                 definition = function(x,value) {
  x@tracing<- value
  return(x)
})



#' @export
setMethod(f = "TMB_out",
          signature = "staRVe_fit",
          definition = function(x) return(x@TMB_out)
)
#' @export
setReplaceMethod(f = "TMB_out",
                 signature = "staRVe_fit",
                 definition = function(x,value) {
  x@TMB_out<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @export
setMethod(f = "convergence",
          signature = "staRVe_fit",
          definition = function(x) {
  return(convergence(TMB_out(x)))
})

#' @export
setMethod(f = "timing",
          signature = "staRVe_fit",
          definition = function(x) {
  return(timing(tracing(x)))
})
