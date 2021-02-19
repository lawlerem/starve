#' @include classes.R getset.R generics.R staRVe_tracing.R staRVe_model.R TMB_out.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @noRd
setMethod(
  f = "initialize",
  signature = "staRVe_model_fit",
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

#' @export
#' @describeIn staRVe_model_fit Get tracing information
setMethod(f = "tracing",
          signature = "staRVe_model_fit",
          definition = function(x) return(x@tracing)
)
setReplaceMethod(f = "tracing",
                 signature = "staRVe_model_fit",
                 definition = function(x,value) {
  x@tracing<- value
  return(x)
})



#' @export
#' @describeIn staRVe_model_fit Get TMB objects
setMethod(f = "TMB_out",
          signature = "staRVe_model_fit",
          definition = function(x) return(x@TMB_out)
)
setReplaceMethod(f = "TMB_out",
                 signature = "staRVe_model_fit",
                 definition = function(x,value) {
  x@TMB_out<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @export
#' @describeIn staRVe_model_fit Get convergence message
setMethod(f = "convergence",
          signature = "staRVe_model_fit",
          definition = function(x) {
  return(convergence(TMB_out(x)))
})

#' @export
#' @describeIn staRVe_model_fit Get timing information
setMethod(f = "timing",
          signature = "staRVe_model_fit",
          definition = function(x) {
  return(timing(tracing(x)))
})
