#' @include classes.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' Create a \code{staRVe_settings} object
#'
#' @export
setGeneric(name = "staRVe_settings",
           def = function(x,...) standardGeneric("staRVe_settings")
)
#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_settings} instead.
#'
#' @export
#' @rdname staRVe_settings
setMethod(
  f = "initialize",
  signature = "staRVe_settings",
  definition = function(.Object,
                        formula = new("formula"),
                        n_neighbours = numeric,
                        p_far_neighbours = "numeric",
                        distance_units = "character") {
    formula(.Object)<- formula
    n_neighbours(.Object)<- n_neighbours
    p_far_neighbours(.Object)<- p_far_neighbours
    distance_units(.Object)<- distance_units

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_settings}.
#'
#' @param x An object of class \code{staRVe_settings}.
#' @param value A replacement value
#'
#' @family Access_staRVe_settings
#' @name Access_staRVe_settings
NULL

#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "formula",
           def = function(x) standardGeneric("formula")
)
#' @export
setMethod(f = "formula",
          signature = "staRVe_settings",
          definition = function(x) return(x@formula)
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "formula<-",
           def = function(x,value) standardGeneric("formula<-")
)
#' @export
setReplaceMethod(f = "formula",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@formula<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "n_neighbours",
           def = function(x) standardGeneric("n_neighbours")
)
#' @export
setMethod(f = "n_neighbours",
          signature = "staRVe_settings",
          definition = function(x) return(x@n_neighbours)
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "n_neighbours<-",
           def = function(x,value) standardGeneric("n_neighbours<-")
)
#' @export
setReplaceMethod(f = "n_neighbours",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@n_neighbours<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "p_far_neighbours",
           def = function(x) standardGeneric("p_far_neighbours")
)
#' @export
setMethod(f = "p_far_neighbours",
          signature = "staRVe_settings",
          definition = function(x) return(x@p_far_neighbours)
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "p_far_neighbours<-",
           def = function(x,value) standardGeneric("p_far_neighbours<-")
)
#' @export
setReplaceMethod(f = "p_far_neighbours",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@p_far_neighbours<- value
  return(x)
})



#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "distance_units",
           def = function(x) standardGeneric("distance_units")
)
#' @export
setMethod(f = "distance_units",
          signature = "staRVe_settings",
          definition = function(x) return(x@distance_units)
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "distance_units<-",
           def = function(x,value) standardGeneric("distance_units<-")
)
#' @export
setReplaceMethod(f = "distance_units",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@distance_units<- value
  return(x)
})
