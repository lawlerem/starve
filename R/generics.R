#' @include classes.R getset.R
NULL


###
### Collection of all generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' Convert a model object into a form suitable for TMB input.
#'
#' @return A list with elements data, para, map, and rand to supply to TMB::MakeADFun
#'
#' @noRd
setGeneric(name = "convert_to_TMB_list",
           def = function(x) standardGeneric("convert_to_TMB_list")
)

#' Create an index connecting locations to a graph
#'
#' @param x An object with locations
#' @param y An object with graph locations
#'
#' @return An integer vector with size equal to the number of rows of x.
#'   The location of row i of x will be the same location as locations
#'   answer[i] of the graph of y.
#'
#' @noRd
setGeneric(name = "create_graph_idx",
           def = function(x,y,...) standardGeneric("create_graph_idx")
)




# D




# E





# F





# G






# H




# I

#' Exchange indices between C++ and R
#'
#' Converts indices to be used in C++ (where indices start at 0) and R
#'   (where indices start at 1).
#'
#' @param x An object with numeric entries
#'
#' @name idx_exchange
#' @noRd
NULL

# #' @describeIn idx_exchange Add 1 to all indices to convert from C++ to R
#' @noRd
setGeneric(name = "convert_idxC_to_R",
           def = function(x) standardGeneric("convert_idxC_to_R")
)
# #' @describeIn idx_exchange Subtract 1 from all indices to convert from R to C++
#' @noRd
setGeneric(name = "convert_idxR_to_C",
           def = function(x) standardGeneric("convert_idxR_to_C")
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

#' Fit a model object.
#'
#' Takes an unfitted model object and performs inference.
#'
#' @param object A model object to be fitted.
#' @param ... Options to be passed
#'
#' @return A fitted model object
#'
#' @export
setGeneric(name = "strv_fit",
           def = function(object,...) standardGeneric("strv_fit")
)


#' Use a fitted model to predict.
#'
#' @param x A fitted model
#' @param new_data The new set of data for which to predict
#' @param ... Extra options
#'
#' @return Predictions for the new data
#'
#' @export
setGeneric(name = "strv_predict",
           def = function(x,new_data,...) standardGeneric("strv_predict")
)

#' Simulate from a model object.
#'
#' @param object A model object to simulate from.
#' @param ... Extra options
#'
#' @return A single copy of \code{object} with a simulated dataset replacing
#'   the original data
#'
#' @export
setGeneric(name = "strv_simulate",
           def = function(object,...) standardGeneric("strv_simulate")
)

#' Update a model from a fitted object
#'
#' @param x The model to be updated
#' @param y The object to update the model with.
#'
#' @return An updated copy of x
#'
#' @noRd
setGeneric(name = "strv_update",
           def = function(x,y) standardGeneric("strv_update")
)




# T





# U






# V





# W





# X





# Y





# Z
