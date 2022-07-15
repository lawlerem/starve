#' @include classes.R getset.R
NULL


###
### Collection of all generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' @noRd
setGeneric(name = ".create_graph_idx",
           def = function(x,y,...) standardGeneric(".create_graph_idx")
)




# D




# E





# F





# G






# H




# I

#' @noRd
setGeneric(name = "idxC_to_R",
           def = function(x) standardGeneric("idxC_to_R")
)
#' @noRd
setGeneric(name = "idxR_to_C",
           def = function(x) standardGeneric("idxR_to_C")
)




# J





# K





# L





# M





# N





# O





# P




# Q





# R





# S

#' Fit a \code{staRVe_model} object.
#'
#' After creating a \code{staRVe_model} object using \code{prepare_staRVe_model},
#'   this function is used to find maximum likelihood estimates of the parameters
#'   and random effects.
#'
#' @param x A staRVe_model object.
#' @param ... Extra options
#'
#' @return A staRVe_fit object.
#'
#' @export
setGeneric(name = "staRVe_fit",
           def = function(x,...) standardGeneric("staRVe_fit")
)



#' Predict from a \code{staRVe_fit} object.
#'
#' @param x An object of class \code{staRVe_fit}.
#' @param locations Either an object of class \code{sf} containing point geometries,
#'  or an object of class \code{RasterLayer}. If an \code{sf} object, covariate
#'  values and a time index should be included as part of the object.
#'  If a \code{RasterLayer} object, predictions will be made for all raster cells
#'  whose value are not NA. If the raster has no values, then predictions will
#'  be made at every cell. Raster predictions are made at the midpoint of each cell.
#' @param covariates A list of \code{Raster*} objects for raster predictions.
#'  If the model has no covariates, then nothing needs to be supplied.
#'
#'  If \code{locations} is of class \code{RasterLayer}, then \code{covariates}
#'  should be a list of \code{Raster*} objects. Each \code{Raster*} object should
#'  contain data for one covariate, should have one layer for each time unit,
#'  and should have the same raster geometry as the \code{locations} object. The
#'  layer names of each raster layer should be of the form \code{T####}, where
#'  \code{####} gives the specific time index. The geometry of all the
#'  \code{Raster*} objects should be identical.
#'
#'  #'  If \code{locations} is of class \code{sf} with point geometries, then
#'  covariate values should be included in that \code{sf} object.
#' @param time What time indices should predictions be made for raster prediction?
#'  If set to "model", predictions are made for every time present in the model.
#' @param ... Extra options
#'
#' @return Either a \code{sf} object or a list of \code{Raster*} objects,
#'  containing predictions (with standard errors) for the spatial field, and the linear
#'  predictor on the link and response scales.
#'  The return type is the same as the type of input for \code{locations}.
#'
#' @export
setGeneric(name = "staRVe_predict",
           def = function(x,locations,covariates,time="model",...) standardGeneric("staRVe_predict")
)

#' Simulate from a staRVe_model.
#'
#' Simulate a new dataset from the model, with the option to simulate a new
#' set of random effects as well. The parameter values used in the simulations
#' are those set in \code{parameters(model)}. Unless new random effects are simulated,
#' the random effect values are those in \code{time_effects(model)} and
#' \code{random_effects(model)}.
#'
#' @param model A staRVe_model object.
#' @param ... Extra options
#'
#' @return A staRve_model object with simulated random effects and observations.
#'
#' @export
setGeneric(name = "staRVe_simulate",
           def = function(model,...) standardGeneric("staRVe_simulate")
)




# T

setGeneric(name = ".time_name",
           def = function(x) standardGeneric(".time_name")
)

#' Convert a staRVe_model object to a form suitable for TMB input.
#'
#' @return A list with elements data, para, map, and rand to supply to TMB::MakeADFun
#'
#' @noRd
setGeneric(name = "TMB_in",
           def = function(x) standardGeneric("TMB_in")
)




# U

#' Update staRVe_model parameters / random effects from a fitted TMB::MakeADFUn object
#'
#' @return A staRVe_model object with ML estimates
#'
#' @noRd
setGeneric(name = "update_staRVe_model",
           def = function(x,y) standardGeneric("update_staRVe_model")
)





# V





# W





# X





# Y





# Z
