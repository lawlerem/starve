#' @include classes.R generics.R dag.R observation_parameters.R process.R long_stars.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param data_predictions A long_stars object
#' @param transient_graph A dag object
#' @param parameters An observation_parameters object
#'
#' @noRd
setMethod(
  f = "initialize",
  signature = "observations",
  definition = function(
    .Object,
    data_predictions = new("long_stars"),
    parameters = new("observation_parameters")) {
  data_predictions(.Object)<- data_predictions
  parameters(.Object)<- parameters

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
#' @describeIn observations_class Get data, including response variables,
#'   time indices, locations, covariates, etc.
setMethod(
    f = "dat",
    signature = "observations",
    definition = function(x) {
  return(locations(x@data_predictions))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn observations_class Set data. Warning: if you want to add new
#'   rows you must give each additional row a value in the graph_idx column,
#'   or create a new object from scratch.
setReplaceMethod(
    f = "dat",
    signature = "observations",
    definition = function(x, value) {
  locations(x@data_predictions)<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn observations_class Get data_predictions, a long_stars object
#'   with the data (see \code{dat}) and associated random effect predictions.
setMethod(
    f = "data_predictions",
    signature = "observations",
    definition = function(x) {
  return(x@data_predictions)
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn observations_class Set data predictions
setReplaceMethod(
    f = "data_predictions",
    signature = "observations",
    definition = function(x, value) {
  x@data_predictions<- value
  return(x)
})




#' @param x An object
#'
#' @export
#' @describeIn observations_class Get parameters as an
#'   observation_parameters object.
setMethod(
    f = "parameters",
    signature = "observations",
    definition = function(x) {
  return(x@parameters)
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn observations_class Set parameters using a new
#'   observation_parameters object. Not the recommended way to
#'   modify specific parmaeter values, instead see the package vignette
setReplaceMethod(
    f = "parameters",
    signature = "observations",
    definition = function(x, value) {
  x@parameters<- value
  return(x)
})
