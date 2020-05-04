#' @include classes.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

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
                        n_neighbours = 10,
                        p_far_neighbours = 0.2,
                        distance_units = "km",
                        max_distance = Inf) {
    formula(.Object)<- formula
    n_neighbours(.Object)<- n_neighbours
    p_far_neighbours(.Object)<- p_far_neighbours
    distance_units(.Object)<- distance_units
    max_distance(.Object)<- max_distance

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
setMethod(f = "formula",
          signature = "staRVe_settings",
          definition = function(x) return(x@formula)
)
#' @export
setReplaceMethod(f = "formula",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@formula<- value
  return(x)
})



#' @export
setMethod(f = "n_neighbours",
          signature = "staRVe_settings",
          definition = function(x) return(x@n_neighbours)
)
#' @export
setReplaceMethod(f = "n_neighbours",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@n_neighbours<- value
  return(x)
})



#' @export
setMethod(f = "p_far_neighbours",
          signature = "staRVe_settings",
          definition = function(x) return(x@p_far_neighbours)
)
#' @export
setReplaceMethod(f = "p_far_neighbours",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@p_far_neighbours<- value
  return(x)
})



#' @export
setMethod(f = "distance_units",
          signature = "staRVe_settings",
          definition = function(x) return(x@distance_units)
)
#' @export
setReplaceMethod(f = "distance_units",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@distance_units<- value
  return(x)
})



#' @export
setMethod(f = "max_distance",
          signature = "staRVe_settings",
          definition = function(x) return(x@max_distance)
)
#' @export
setReplaceMethod(f = "max_distance",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@max_distance<- value
  return(x)
})
