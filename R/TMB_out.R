#' @include classes.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{TMB_out} instead.
#'
#' @export
#' @noRd
setMethod(
  f = "initialize",
  signature = "TMB_out",
  definition = function(.Object,
                        obj = list(),
                        opt = list(),
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

#' Get or set slots from an object of class \code{TMB_out}.
#'
#' @param x An object of class \code{TMB_out}.
#' @param value A replacement value
#'
#' @family access_TMB_out
#' @name access_TMB_out
NULL

#' @export
#' @rdname access_TMB_out
setMethod(f = "obj",
          signature = "TMB_out",
          definition = function(x) return(x@obj)
)
#' @export
#' @rdname access_TMB_out
setReplaceMethod(f = "obj",
                 signature = "TMB_out",
                 definition = function(x,value) {
  x@obj<- value
  return(x)
})

#' @export
#' @rdname access_TMB_out
setMethod(f = "opt",
          signature = "TMB_out",
          definition = function(x) return(x@opt)
)
#' @export
#' @rdname access_TMB_out
setReplaceMethod(f = "opt",
                 signature = "TMB_out",
                 definition = function(x,value) {
  x@opt<- value
  return(x)
})

#' @export
#' @rdname access_TMB_out
setMethod(f = "sdr",
          signature = "TMB_out",
          definition = function(x) return(x@sdr)
)
#' @export
#' @rdname access_TMB_out
setReplaceMethod(f = "sdr",
                 signature = "TMB_out",
                 definition = function(x,value) {
  x@sdr<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @export
#' @rdname access_TMB_out
setMethod(f = "convergence",
          signature = "TMB_out",
          definition = function(x) {
  return(opt(x)$message)
})
