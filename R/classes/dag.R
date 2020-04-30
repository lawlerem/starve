#' @include classes.R
NULL

#####################
###               ###
### Construct_dag ###
###               ###
#####################

#' Create a \code{dag} object
#'
#' @export
setGeneric(name = "dag",
           def = function(x,...) standardGeneric("dag")
)
#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{dag} instead.
#'
#' @export
#' @rdname dag
setMethod(
  f = "initialize",
  signature = "dag",
  definition = function(.Object,
                        edges = list(),
                        distances = list()) {
    edges(.Object)<- edges
    distances(.Object)<- distances

    return(.Object)
  }
)


##################
###            ###
### Access_dag ###
###            ###
##################

#' Get or set slots from an object of class \code{dag}.
#'
#' @param x An object of class \code{dag}.
#' @param value A replacement value.
#'
#' @family Access_dag
#' @name Access_dag
NULL

#' @export
#' @rdname Access_dag
setGeneric(name = "edges",
           def = function(x) standardGeneric("edges")
)
#' @export
setMethod(f = "edges",
          signature = "dag",
          definition = function(x) return(x@edges)
)
#' @export
#' @rdname Access_dag
setGeneric(name = "edges<-",
           def = function(x,value) standardGeneric("edges<-")
)
#' @export
setReplaceMethod(f = "edges",
                 signature = "dag",
                 definition = function(x,value) {
  x@edges<- value
  return(x)
})



#' @export
#' @rdname Access_dag
setGeneric(name = "distances",
           def = function(x) standardGeneric("distances")
)
#' @export
setMethod(f = "distances",
          signature = "dag",
          definition = function(x) return(x@distances)
)
#' @export
#' @rdname Access_dag
setGeneric(name = "distances<-",
           def = function(x,value) standardGeneric("distances<-")
)
#' @export
setReplaceMethod(f = "distances",
                 signature = "dag",
                 definition = function(x,value) {
  x@distances<- value
  return(x)
})
