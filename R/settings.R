#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param formula A formula
#' @param n_neighbours An integer
#' @param distance_units Which distance units to use
#'
#' @noRd
setMethod(
    f = "initialize",
    signature = "settings",
    definition = function(
      .Object,
      formula = new("formula"),
      n_neighbours = 10,
      distance_units = "km",
      max_distance = Inf) {
  formula(.Object)<- formula
  n_neighbours(.Object)<- n_neighbours
  .Object@distance_units<- distance_units
  max_distance(.Object)<- max_distance

  return(.Object)
})



##############
###        ###
### Access ###
###        ###
##############


#' @param x An object
#'
#' @export
#' @describeIn settings_class Get model formula
setMethod(
    f = "formula",
    signature = "settings",
    definition = function(x) {
  return(x@formula)
})

#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn settings_class Set model formula
setReplaceMethod(
    f = "formula",
    signature = "settings",
    definition = function(x, value) {
  x@formula<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn settings_class Get maximum number of neighbours used in graphs
setMethod(
    f = "n_neighbours",
    signature = "settings",
    definition = function(x) {
  return(x@n_neighbours)
})

# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn settings_class Set maximum number of neighbours used in graphs.
setReplaceMethod(
    f = "n_neighbours",
    signature = "settings",
    definition = function(x, value) {
  x@n_neighbours<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn settings_class Get distance units used for the model.
setMethod(
    f = "distance_units",
    signature = "settings",
    definition = function(x) {
  return(x@distance_units)
})

#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn settings_class Set distance units. Automatically converts
#'   max_distance setting to new units.
setReplaceMethod(
    f = "distance_units",
    signature = "settings",
    definition = function(x, value) {
  max_distance<- units::set_units(
    max_distance(x),
    distance_units(x),
    mode = "standard"
  )
  x@distance_units<- value
  max_distance<- units::set_units(
    max_distance,
    value,
    mode = "standard"
  )
  max_distance(x)<- units::drop_units(max_distance)
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn settings_class Get maximum distance for edges in the graph
setMethod(
    f = "max_distance",
    signature = "settings",
    definition = function(x) {
  return(x@max_distance)
})

#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn settings_class Set maximum distance for edges in the graph
setReplaceMethod(
    f = "max_distance",
    signature = "settings",
    definition = function(x, value) {
  x@max_distance<- value
  return(x)
})



# #' @param x An object
# #'
# #' @describeIn settings_class Get the name of the time variable used in
# #'   formula
setMethod(
    f = "time_name",
    signature = "settings",
    definition = function(x) {
  return(time_name(formula(x)))
})
