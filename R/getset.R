#' @include classes.R
NULL


#' A list of accessor functions for the staRVe package
#'
#' @param x An object
#' @param value A replacement value
#'
#' @name staRVe-access
NULL



###
### Collection of all get/set generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' @describeIn staRVe-access Get convergence message
#' @export
setGeneric(name = "convergence",
           def = function(x) standardGeneric("convergence")
)

#' @describeIn staRVe-access Get covariance function
#' @export
setGeneric(name = "covariance_function",
           def = function(x) standardGeneric("covariance_function")
)
#' @describeIn staRVe-access Set covariance function
#' @export
setGeneric(name = "covariance_function<-",
           def = function(x,value) standardGeneric("covariance_function<-")
)




# D

#' @describeIn staRVe-access Get data
#' @export
setGeneric(name = "dat",
           def = function(x) standardGeneric("dat")
)
#' @describeIn staRVe-access Set data
#' @export
setGeneric(name = "dat<-",
           def = function(x,value) standardGeneric("dat<-")
)

#' @describeIn staRVe-access Get data predictions
#' @export
setGeneric(name = "data_predictions",
           def = function(x,...) standardGeneric("data_predictions")
)
#' @describeIn staRVe-access Set data
#' @export
setGeneric(name = "data_predictions<-",
           def = function(x,value) standardGeneric("data_predictions<-")
)

#' @describeIn staRVe-access Get distance units
#' @export
setGeneric(name = "distance_units",
           def = function(x) standardGeneric("distance_units")
)
#' @describeIn staRVe-access Set distance units
#' @export
setGeneric(name = "distance_units<-",
           def = function(x,value) standardGeneric("distance_units<-")
)

#' @describeIn staRVe-access Get distances
#' @export
setGeneric(name = "distances",
           def = function(x) standardGeneric("distances")
)
#' @noRd
setGeneric(name = "distances<-",
           def = function(x,value) standardGeneric("distances<-")
)





# E

#' @describeIn staRVe-access Get edges2
#' @export
setGeneric(name = "edges",
           def = function(x) standardGeneric("edges")
)
#' @noRd
setGeneric(name = "edges<-",
           def = function(x,value) standardGeneric("edges<-")
)





# F

#' @describeIn staRVe-access Get fixed effects
#' @export
setGeneric(name = "fixed_effects",
           def = function(x) standardGeneric("fixed_effects")
)
#' @describeIn staRVe-access Set fixed effects
#' @export
setGeneric(name = "fixed_effects<-",
           def = function(x,value) standardGeneric("fixed_effects<-")
)

#' @describeIn staRVe-access Get formula
#' @export
setGeneric(name = "formula",
           def = function(x) standardGeneric("formula")
)
#' @describeIn staRVe-access Set formula
#' @export
setGeneric(name = "formula<-",
           def = function(x,value) standardGeneric("formula<-")
)




# G

#' @describeIn staRVe-access Get graph
#' @export
setGeneric(name = "graph",
           def = function(x) standardGeneric("graph")
)



# H

#' @describeIn staRVe-access Get hessian time
#' @export
setGeneric(name = "hess_time",
           def = function(x) standardGeneric("hess_time")
)
#' @noRd
setGeneric(name = "hess_time<-",
           def = function(x,value) standardGeneric("hess_time<-")
)




# I



# J



# K



# L

#' @describeIn staRVe-access Get link function
#' @export
setGeneric(name = "link_function",
           def = function(x) standardGeneric("link_function")
)
#' @describeIn staRVe-access Set link function
#' @export
setGeneric(name = "link_function<-",
           def = function(x,value) standardGeneric("link_function<-")
)

#' @describeIn staRVe-access Get locations
#' @export
setGeneric(name = "locations",
           def = function(x) standardGeneric("locations")
)
#' @describeIn staRVe-access Get locations
#' @export
setGeneric(name = "locations<-",
           def = function(x,value) standardGeneric("locations<-")
)




# M

#' @describeIn staRVe-access Get max distance
#' @export
setGeneric(name = "max_distance",
           def = function(x) standardGeneric("max_distance")
)
#' @noRd
setGeneric(name = "max_distance<-",
           def = function(x,value) standardGeneric("max_distance<-")
)




# N

#' @describeIn staRVe-access Get n neighbours
#' @export
setGeneric(name = "n_neighbours",
           def = function(x) standardGeneric("n_neighbours")
)
#' @noRd
setGeneric(name = "n_neighbours<-",
           def = function(x,value) standardGeneric("n_neighbours<-")
)




# O

#' @describeIn staRVe-access Get obj
#' @export
setGeneric(name = "obj",
  def = function(x) standardGeneric("obj")
)
#' @noRd
setGeneric(name = "obj<-",
  def = function(x,value) standardGeneric("obj<-")
)


#' @noRd
setGeneric(name = "observations",
           def = function(x) standardGeneric("observations")
)
#' @noRd
setGeneric(name = "observations<-",
           def = function(x,value) standardGeneric("observations<-")
)

#' @describeIn staRVe-access Get opt
#' @export
setGeneric(name = "opt",
  def = function(x) standardGeneric("opt")
)
#' @noRd
setGeneric(name = "opt<-",
  def = function(x,value) standardGeneric("opt<-")
)

#' @describeIn staRVe-access Get opt time
#' @export
setGeneric(name = "opt_time",
           def = function(x) standardGeneric("opt_time")
)
#' @noRd
setGeneric(name = "opt_time<-",
           def = function(x,value) standardGeneric("opt_time<-")
)




# P

#' @describeIn staRVe-access Get parameter covariance
#' @export
setGeneric(name = "parameter_covariance",
           def = function(x) standardGeneric("parameter_covariance")
)
#' @noRd
setGeneric(name = "parameter_covariance<-",
           def = function(x,value) standardGeneric("parameter_covariance<-")
)

#' @describeIn staRVe-access Get parameter hessian
#' @export
setGeneric(name = "parameter_hessian",
           def = function(x) standardGeneric("parameter_hessian")
)
#' @noRd
setGeneric(name = "parameter_hessian<-",
           def = function(x,value) standardGeneric("parameter_hessian<-")
)

#' @describeIn staRVe-access Get parameters
#' @export
setGeneric(name = "parameters",
           def = function(x) standardGeneric("parameters")
)
#' @describeIn staRVe-access Set parameters
#' @export
setGeneric(name = "parameters<-",
           def = function(x,value) standardGeneric("parameters<-")
)

#' @noRd
setGeneric(name = "persistent_graph",
           def = function(x) standardGeneric("persistent_graph")
)
#' @noRd
setGeneric(name = "persistent_graph<-",
           def = function(x,value) standardGeneric("persistent_graph<-")
)

#' @describeIn staRVe-access Get persistent graph random effects
#' @export
setGeneric(name = "pg_re",
           def = function(x) standardGeneric("pg_re")
)
#' @describeIn staRVe-access Set persistent graph random effects
#' @export
setGeneric(name = "pg_re<-",
           def = function(x,value) standardGeneric("pg_re<-")
)

#' @noRd
setGeneric(name = "process",
           def = function(x) standardGeneric("process")
)
#' @noRd
setGeneric(name = "process<-",
           def = function(x,value) standardGeneric("process<-")
)




# Q



# R

#' @describeIn staRVe-access Get random effects
#' @export
setGeneric(name = "random_effects",
           def = function(x) standardGeneric("random_effects")
)

#' @describeIn staRVe-access Get response distribution
#' @export
setGeneric(name = "response_distribution",
           def = function(x) standardGeneric("response_distribution")
)
#' @describeIn staRVe-access Set response distribution
#' @export
setGeneric(name = "response_distribution<-",
           def = function(x,value) standardGeneric("response_distribution<-")
)

#' @describeIn staRVe-access Get response parameters
#' @export
setGeneric(name = "response_parameters",
           def = function(x) standardGeneric("response_parameters")
)
#' @describeIn staRVe-access Set response parameters
#' @export
setGeneric(name = "response_parameters<-",
           def = function(x,value) standardGeneric("response_parameters<-")
)




# S

#' @describeIn staRVe-access Get sdr
#' @export
setGeneric(name = "sdr",
           def = function(x) standardGeneric("sdr")
)
#' @noRd
setGeneric(name = "sdr<-",
           def = function(x,value) standardGeneric("sdr<-")
)

#' @describeIn staRVe-access Get sdr time
#' @export
setGeneric(name = "sdr_time",
           def = function(x) standardGeneric("sdr_time")
)
#' @noRd
setGeneric(name = "sdr_time<-",
           def = function(x,value) standardGeneric("sdr_time<-")
)

#' @describeIn staRVe-access Get settings
#' @export
setGeneric(name = "settings",
           def = function(x) standardGeneric("settings")
)
#' @noRd
setGeneric(name = "settings<-",
           def = function(x,value) standardGeneric("settings<-")
)

#' @describeIn staRVe-access Get spatial parameters
#' @export
setGeneric(name = "spatial_parameters",
           def = function(x) standardGeneric("spatial_parameters")
)
#' @describeIn staRVe-access Set spatial parameters
#' @export
setGeneric(name = "spatial_parameters<-",
           def = function(x,value) standardGeneric("spatial_parameters<-")
)




# T

#' @describeIn staRVe-access Get transient graph random effects
#' @export
setGeneric(name = "tg_re",
           def = function(x) standardGeneric("tg_re")
)
#' @describeIn staRVe-access Set transient graph random effects
#' @export
setGeneric(name = "tg_re<-",
           def = function(x,value) standardGeneric("tg_re<-")
)

#' @describeIn staRVe-access Get time effects
#' @export
setGeneric(name = "time_effects",
           def = function(x) standardGeneric("time_effects")
)
#' @describeIn staRVe-access Set time effects
#' @export
setGeneric(name = "time_effects<-",
           def = function(x,value) standardGeneric("time_effects<-")
)

#' @describeIn staRVe-access Get time name used
#' @keywords internal
setGeneric(name = ".time_name",
           def = function(x) standardGeneric(".time_name")
)


#' @describeIn staRVe-access Get time parameters
#' @export
setGeneric(name = "time_parameters",
           def = function(x) standardGeneric("time_parameters")
)
#' @describeIn staRVe-access Set time parameters
#' @export
setGeneric(name = "time_parameters<-",
           def = function(x,value) standardGeneric("time_parameters<-")
)

#' @describeIn staRVe-access Get timing
#' @export
setGeneric(name = "timing",
           def = function(x) standardGeneric("timing")
)

#' @describeIn staRVe-access Get TMB_out
#' @export
setGeneric(name = "TMB_out",
           def = function(x) standardGeneric("TMB_out")
)
#' @noRd
setGeneric(name = "TMB_out<-",
           def = function(x,value) standardGeneric("TMB_out<-")
)

#' @describeIn staRVe-access Get tracing
#' @export
setGeneric(name = "tracing",
           def = function(x) standardGeneric("tracing")
)
#' @noRd
setGeneric(name = "tracing<-",
           def = function(x,value) standardGeneric("tracing<-")
)

#' @noRd
setGeneric(name = "transient_graph",
           def = function(x) standardGeneric("transient_graph")
)
#' @noRd
setGeneric(name = "transient_graph<-",
           def = function(x,value) standardGeneric("transient_graph<-")
)




# U



# V

#' @describeIn staRVe-access Get values
#' @export
setGeneric(name = "values",
           def = function(x) standardGeneric("values")
)
#' @describeIn staRVe-access Set values
#' @export
setGeneric(name = "values<-",
           def = function(x,value) standardGeneric("values<-")
)




# W



# X



# Y



# Z
