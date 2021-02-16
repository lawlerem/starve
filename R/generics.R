#' @include classes.R getset.R
NULL

#' A list of generics for the staRVe package.
#'
#' To see a list of all methods available for a specific class,
#'   e.g. the staRVe_model class, run \code{methods(class="staRVe_model")}.
#'
#' @name staRVe_generics
NULL



###
### Collection of all generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C





# D




# E





# F





# G






# H




# I

#' @export
#' @rdname staRVe_generics
setGeneric(name = "idxC_to_R",
def = function(x) standardGeneric("idxC_to_R")
)
#' @export
#' @rdname staRVe_generics
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

#' @export
#' @rdname staRVe_generics
setGeneric(name = "report",
           def = function(x) standardGeneric("report")
)





# S

#' @export
#' @rdname staRVe_generics
setGeneric(name = "sdreport",
           def = function(x) standardGeneric("sdreport")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_fit",
           def = function(x,...) standardGeneric("staRVe_fit")
)



#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_predict",
           def = function(x,locations,...) standardGeneric("staRVe_predict")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_simulate",
           def = function(model,...) standardGeneric("staRVe_simulate")
)




# T

#' @export
#' @rdname staRVe_generics
setGeneric(name = "TMB_in",
           def = function(x) standardGeneric("TMB_in")
)




# U

#' @export
#' @rdname staRVe_generics
setGeneric(name = "update_staRVe_model",
           def = function(x,y) standardGeneric("update_staRVe_model")
)





# V





# W





# X





# Y





# Z
