#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param formula A formula
#' @param n_neighbours An integer
#' @param p_far_neighbours A number between 0 and 1
#' @param distance_units Which distance units to use
#' @param max_distance A positive number
#' @param obs_dag_method The method used to create the transient graph
#' @param extras A list of extra settings
#'
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "staRVe_settings",
  definition = function(.Object,
                        formula = new("formula"),
                        n_neighbours = 10,
                        p_far_neighbours = 0,
                        distance_units = "km",
                        max_distance = Inf,
                        obs_dag_method = "standard",
                        extras = vector(mode="list",length=0)) {
    formula(.Object)<- formula
    n_neighbours(.Object)<- n_neighbours
    p_far_neighbours(.Object)<- p_far_neighbours
    .Object@distance_units<- distance_units
    max_distance(.Object)<- max_distance
    obs_dag_method(.Object)<- obs_dag_method
    extras<- extras

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
#' @describeIn staRVe_settings Get model formula
setMethod(f = "formula",
          signature = "staRVe_settings",
          definition = function(x) return(x@formula)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_settings Set formula
setReplaceMethod(f = "formula",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@formula<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_settings Get maximum number of neighbours
setMethod(f = "n_neighbours",
          signature = "staRVe_settings",
          definition = function(x) return(x@n_neighbours)
)
setReplaceMethod(f = "n_neighbours",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@n_neighbours<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_settings Get percentage of far neighbours
setMethod(f = "p_far_neighbours",
          signature = "staRVe_settings",
          definition = function(x) return(x@p_far_neighbours)
)
setReplaceMethod(f = "p_far_neighbours",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@p_far_neighbours<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_settings Get distance units
setMethod(f = "distance_units",
          signature = "staRVe_settings",
          definition = function(x) return(x@distance_units)
)
#' @param x An object
#' @param value A replacement value
#"
#' @export
#' @describeIn staRVe_settings Set distance units
setReplaceMethod(f = "distance_units",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  max_distance<- units::set_units(max_distance(x),distance_units(x),mode="standard")
  x@distance_units<- value

  max_distance<- units::set_units(max_distance,value,mode="standard")
  max_distance(x)<- units::drop_units(max_distance)
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_settings Get maximum distance for neighbours
setMethod(f = "max_distance",
          signature = "staRVe_settings",
          definition = function(x) return(x@max_distance)
)
setReplaceMethod(f = "max_distance",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@max_distance<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_settings Get obs_dag_method
setMethod(f = "obs_dag_method",
          signature = "staRVe_settings",
          definition = function(x) return(x@obs_dag_method)
)
setReplaceMethod(f = "obs_dag_method",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@obs_dag_method<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_settings Get extra settings
setMethod(f = "extras",
          signature = "staRVe_settings",
          definition = function(x) return(x@extras)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_settings Set miscellaneous settings
setReplaceMethod(f = "extras",
                 signature = "staRVe_settings",
                 definition = function(x,value) {
  x@extras<- value
  return(x)
})



setMethod(f = ".time_name",
          signature = "staRVe_settings",
          definition = function(x) {
  return(.time_name_from_formula(formula(x)))
})
