#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param obj Output of TMB::MakeADFun
#' @param opt Output of nlminb
#' @param sdr Output of TMB::sdreport
#'
#' @rdname starve-construct
setMethod(
  f = "initialize",
  signature = "TMB_out",
  definition = function(.Object,
                        obj = list(),
                        opt = list(convergence = 0,message="Model not yet fitted."),
                        sdr = structure(numeric(0),class="sdreport")) {
    obj(.Object)<- obj
    opt(.Object)<- opt
    sdr(.Object)<- sdr

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
#' @describeIn TMB_out_class Get TMB::MakeADFun object
setMethod(f = "obj",
          signature = "TMB_out",
          definition = function(x) return(x@obj)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn TMB_out_class Set TMB::MakeADFun object
setReplaceMethod(f = "obj",
                 signature = "TMB_out",
                 definition = function(x,value) {
  x@obj<- value
  return(x)
})

#' @param x An object
#'
#' @describeIn TMB_out_class Get output of optimizer
setMethod(f = "opt",
          signature = "TMB_out",
          definition = function(x) return(x@opt)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn TMB_out_class Set output of optimizer
setReplaceMethod(f = "opt",
                 signature = "TMB_out",
                 definition = function(x,value) {
  x@opt<- value
  return(x)
})

#' @param x An object
#'
#' @describeIn TMB_out_class Get output of TMB::sdreport
setMethod(f = "sdr",
          signature = "TMB_out",
          definition = function(x) return(x@sdr)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn TMB_out_class Set output of TMB::sdreport
setReplaceMethod(f = "sdr",
                 signature = "TMB_out",
                 definition = function(x,value) {
  x@sdr<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @param x An object
#'
#' @describeIn TMB_out_class Get convergence message from optimizer
setMethod(f = "convergence",
          signature = "TMB_out",
          definition = function(x) {
  return(opt(x)$message)
})
