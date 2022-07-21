#' @include classes.R getset.R
NULL


###
### Collection of all generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' Create an index connecting locations to a graph
#'
#' @param x An object with locations
#' @param y An object with graph locations
#'
#' @return An integer vector with size equal to the number of rows of x. The location of row i of x will be the same
#'   location as locations answer[i] of the graph of y.
#'
#' @keywords internal
setGeneric(name = ".create_graph_idx",
           def = function(x,y,...) standardGeneric(".create_graph_idx")
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
#' @keywords internal
#'
#' @name idx_exchange
NULL

#' @describeIn idx_exchange Add 1 to all indices to convert from C++ to R
setGeneric(name = "idxC_to_R",
           def = function(x) standardGeneric("idxC_to_R")
)
#' @describeIn idx_exchange Subtract 1 from all indices to convert from R to C++
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

#' Fit a model object.
#'
#' Takes an unfitted model object and performs inference.
#'
#' @param x A model object to be fitted.
#' @param ... Options to be passed
#'
#' @return A fitted model object.
#'
#' @export
setGeneric(name = "staRVe_fit",
           def = function(x,...) standardGeneric("staRVe_fit")
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
setGeneric(name = "staRVe_predict",
           def = function(x,new_data,...) standardGeneric("staRVe_predict")
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
setGeneric(name = "staRVe_simulate",
           def = function(object,...) standardGeneric("staRVe_simulate")
)




# T


#' Convert an into a form suitable for TMB input.
#'
#' @return A list with elements data, para, map, and rand to supply to TMB::MakeADFun
#'
#' @keywords internal
setGeneric(name = "TMB_in",
           def = function(x) standardGeneric("TMB_in")
)




# U

#' Update a model from a fitted object
#'
#' @param x The model to be updated
#' @param y The object to update the model with.
#'
#' @return An updated copy of x
#'
#' @keywords internal
setGeneric(name = "update_staRVe_model",
           def = function(x,y) standardGeneric("update_staRVe_model")
)





# V





# W





# X





# Y





# Z
