#' @include classes.R staRVe_tracing.R staRVe_model.R Access_TMB_out.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' Create a \code{staRVe_fit} object
#'
#' @export
setGeneric(name = "staRVe_fit",
           def = function(x,...) standardGeneric("staRVe_fit")
)
#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_fit} instead.
#'
#' @export
#' @rdname staRVe_fit
setMethod(
  f = "initialize",
  signature = "staRVe_fit",
  definition = function(.Object,
                        staRVe_model = new("staRVe_model",
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
#' @rdname Access_staRVe_fit
setGeneric(name = "tracing",
           def = function(x) standardGeneric("tracing")
)
#' @export
setMethod(f = "tracing",
          signature = "staRVe_fit",
          definition = function(x) return(x@tracing)
)
#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "tracing<-",
           def = function(x,value) standardGeneric("tracing<-")
)
#' @export
setReplaceMethod(f = "tracing",
                 signature = "staRVe_fit",
                 definition = function(x,value) {
  x@tracing<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "TMB_out",
           def = function(x) standardGeneric("TMB_out")
)
#' @export
setMethod(f = "TMB_out",
          signature = "staRVe_fit",
          definition = function(x) return(x@TMB_out)
)
#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "TMB_out<-",
           def = function(x,value) standardGeneric("TMB_out<-")
)
#' @export
setReplaceMethod(f = "TMB_out",
                 signature = "staRVe_fit",
                 definition = function(x,value) {
  x@TMB_out<- value
  return(x)
})
