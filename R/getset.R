#' @include classes.R
NULL


#' A list of accessor functions for the starve package
#'
#' These accessor functions are used to extract different components from the
#'   objects used in the starve package. This help page is meant only to catalogue
#'   all the accessor functions available, and as such will be of limited benefit
#'   to package users. Not every accessor function is available for
#'   every class implemented in the package. To see which functions are available
#'   for a class, and a description of the different components of that class
#'   that these functions return, please see the documentation for that class.
#'   The main classes implemented in the starve package are \link{dag},
#'   \link{long_stars}, \link{starve}, \link{tracing}, and
#'   \link{starve_fit}.
#'
#' @param x An object
#' @param value A replacement value
#'
#' @seealso \link{dag}, \link{long_stars}, \link{starve}, \link{tracing},
#'   and \link{starve_fit}.
#'
#' @name starve_access
NULL



###
### Collection of all get/set generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' @describeIn starve_access Get convergence message
#' @export
setGeneric(name = "convergence",
           def = function(x) standardGeneric("convergence")
)

#' @describeIn starve_access Get covariance function
#' @export
setGeneric(name = "covariance_function",
           def = function(x) standardGeneric("covariance_function")
)
#' @describeIn starve_access Set covariance function
#' @export
setGeneric(name = "covariance_function<-",
           def = function(x,value) standardGeneric("covariance_function<-")
)




# D

#' @describeIn starve_access Get data
#' @export
setGeneric(name = "dat",
           def = function(x) standardGeneric("dat")
)
#' @describeIn starve_access Set data
#' @export
setGeneric(name = "dat<-",
           def = function(x,value) standardGeneric("dat<-")
)

#' @describeIn starve_access Get data predictions
#' @export
setGeneric(name = "data_predictions",
           def = function(x,...) standardGeneric("data_predictions")
)
#' @describeIn starve_access Set data
#' @export
setGeneric(name = "data_predictions<-",
           def = function(x,value) standardGeneric("data_predictions<-")
)

#' @describeIn starve_access Get distance units
#' @export
setGeneric(name = "distance_units",
           def = function(x) standardGeneric("distance_units")
)
#' @describeIn starve_access Set distance units
#' @export
setGeneric(name = "distance_units<-",
           def = function(x,value) standardGeneric("distance_units<-")
)

#' @describeIn starve_access Get distances
#' @export
setGeneric(name = "distances",
           def = function(x) standardGeneric("distances")
)
#' @describeIn starve_access Set distances
setGeneric(name = "distances<-",
           def = function(x,value) standardGeneric("distances<-")
)





# E

#' @describeIn starve_access Get edges
#' @export
setGeneric(name = "edges",
           def = function(x) standardGeneric("edges")
)
#' @describeIn starve_access Set edges
setGeneric(name = "edges<-",
           def = function(x,value) standardGeneric("edges<-")
)





# F

#' @describeIn starve_access Get fixed effects
#' @export
setGeneric(name = "fixed_effects",
           def = function(x) standardGeneric("fixed_effects")
)
#' @describeIn starve_access Set fixed effects
#' @export
setGeneric(name = "fixed_effects<-",
           def = function(x,value) standardGeneric("fixed_effects<-")
)

#' @describeIn starve_access Get model formula
#' @export
setGeneric(name = "formula",
           def = function(x) standardGeneric("formula")
)
#' @describeIn starve_access Set model formula
#' @export
setGeneric(name = "formula<-",
           def = function(x,value) standardGeneric("formula<-")
)




# G

#' @describeIn starve_access Get graph
#' @export
setGeneric(name = "graph",
           def = function(x) standardGeneric("graph")
)



# H

#' @describeIn starve_access Get hessian computation time
#' @export
setGeneric(name = "hess_time",
           def = function(x) standardGeneric("hess_time")
)
#' @describeIn starve_access Set hessian computation time
setGeneric(name = "hess_time<-",
           def = function(x,value) standardGeneric("hess_time<-")
)




# I



# J



# K



# L

#' @describeIn starve_access Get link function
#' @export
setGeneric(name = "link_function",
           def = function(x) standardGeneric("link_function")
)
#' @describeIn starve_access Set link function
#' @export
setGeneric(name = "link_function<-",
           def = function(x,value) standardGeneric("link_function<-")
)

#' @describeIn starve_access Get locations
#' @export
setGeneric(name = "locations",
           def = function(x) standardGeneric("locations")
)
#' @describeIn starve_access Get locations
#' @export
setGeneric(name = "locations<-",
           def = function(x,value) standardGeneric("locations<-")
)




# M

#' @describeIn starve_access Get max distance
#' @export
setGeneric(name = "max_distance",
           def = function(x) standardGeneric("max_distance")
)
#' @describeIn starve_access Set max distance
setGeneric(name = "max_distance<-",
           def = function(x,value) standardGeneric("max_distance<-")
)




# N

#' @describeIn starve_access Get n_neighbours setting
#' @export
setGeneric(name = "n_neighbours",
           def = function(x) standardGeneric("n_neighbours")
)
#' @describeIn starve_access Set n_neighbours setting
setGeneric(name = "n_neighbours<-",
           def = function(x,value) standardGeneric("n_neighbours<-")
)




# O

#' @describeIn starve_access Get TMB::MakeADFun object
#' @export
setGeneric(name = "obj",
  def = function(x) standardGeneric("obj")
)
#' @describeIn starve_access Set TMB::MakeADFun object
setGeneric(name = "obj<-",
  def = function(x,value) standardGeneric("obj<-")
)


#' @describeIn starve_access Get observations
setGeneric(name = "observations",
           def = function(x) standardGeneric("observations")
)
#' @describeIn starve_access Set observations
setGeneric(name = "observations<-",
           def = function(x,value) standardGeneric("observations<-")
)

#' @describeIn starve_access Get optimizer object
#' @export
setGeneric(name = "opt",
  def = function(x) standardGeneric("opt")
)
#' @describeIn starve_access Set optimizer object
setGeneric(name = "opt<-",
  def = function(x,value) standardGeneric("opt<-")
)

#' @describeIn starve_access Get optimization time
#' @export
setGeneric(name = "opt_time",
           def = function(x) standardGeneric("opt_time")
)
#' @describeIn starve_access Set optimization time
setGeneric(name = "opt_time<-",
           def = function(x,value) standardGeneric("opt_time<-")
)




# P

#' @describeIn starve_access Get parameter estimator covariance matrix
#' @export
setGeneric(name = "parameter_covariance",
           def = function(x) standardGeneric("parameter_covariance")
)
#' @describeIn starve_access Set parameter estimator covariance matrix
setGeneric(name = "parameter_covariance<-",
           def = function(x,value) standardGeneric("parameter_covariance<-")
)

#' @describeIn starve_access Get parameter estimator hessian matrix
#' @export
setGeneric(name = "parameter_hessian",
           def = function(x) standardGeneric("parameter_hessian")
)
#' @describeIn starve_access Set parameter estimator hessian matrix
setGeneric(name = "parameter_hessian<-",
           def = function(x,value) standardGeneric("parameter_hessian<-")
)

#' @describeIn starve_access Get parameters
#' @export
setGeneric(name = "parameters",
           def = function(x) standardGeneric("parameters")
)
#' @describeIn starve_access Set parameters
#' @export
setGeneric(name = "parameters<-",
           def = function(x,value) standardGeneric("parameters<-")
)

#' @describeIn starve_access Get persistent graph
setGeneric(name = "persistent_graph",
           def = function(x) standardGeneric("persistent_graph")
)
#' @describeIn starve_access Set persistent graph
setGeneric(name = "persistent_graph<-",
           def = function(x,value) standardGeneric("persistent_graph<-")
)

#' @describeIn starve_access Get persistent graph random effects
#' @export
setGeneric(name = "pg_re",
           def = function(x) standardGeneric("pg_re")
)
#' @describeIn starve_access Set persistent graph random effects
#' @export
setGeneric(name = "pg_re<-",
           def = function(x,value) standardGeneric("pg_re<-")
)

#' @describeIn starve_access Get process
setGeneric(name = "process",
           def = function(x) standardGeneric("process")
)
#' @describeIn starve_access Set process
setGeneric(name = "process<-",
           def = function(x,value) standardGeneric("process<-")
)




# Q



# R

#' @describeIn starve_access Get random effects
#' @export
setGeneric(name = "random_effects",
           def = function(x) standardGeneric("random_effects")
)

#' @describeIn starve_access Get response distribution
#' @export
setGeneric(name = "response_distribution",
           def = function(x) standardGeneric("response_distribution")
)
#' @describeIn starve_access Set response distribution
#' @export
setGeneric(name = "response_distribution<-",
           def = function(x,value) standardGeneric("response_distribution<-")
)

#' @describeIn starve_access Get response distribution parameters
#' @export
setGeneric(name = "response_parameters",
           def = function(x) standardGeneric("response_parameters")
)
#' @describeIn starve_access Set response distribution parameters
#' @export
setGeneric(name = "response_parameters<-",
           def = function(x,value) standardGeneric("response_parameters<-")
)




# S

#' @describeIn starve_access Get TMB::sdreport object
#' @export
setGeneric(name = "sdr",
           def = function(x) standardGeneric("sdr")
)
#' @describeIn starve_access Set TMB::sdreport object
setGeneric(name = "sdr<-",
           def = function(x,value) standardGeneric("sdr<-")
)

#' @describeIn starve_access Get standard error computation time
#' @export
setGeneric(name = "sdr_time",
           def = function(x) standardGeneric("sdr_time")
)
#' @describeIn starve_access Set standard error computation time
setGeneric(name = "sdr_time<-",
           def = function(x,value) standardGeneric("sdr_time<-")
)

#' @describeIn starve_access Get settings
#' @export
setGeneric(name = "settings",
           def = function(x) standardGeneric("settings")
)
#' @describeIn starve_access Set settings
setGeneric(name = "settings<-",
           def = function(x,value) standardGeneric("settings<-")
)

#' @describeIn starve_access Get spatial parameters
#' @export
setGeneric(name = "spatial_parameters",
           def = function(x) standardGeneric("spatial_parameters")
)
#' @describeIn starve_access Set spatial parameters
#' @export
setGeneric(name = "spatial_parameters<-",
           def = function(x,value) standardGeneric("spatial_parameters<-")
)




# T

#' @describeIn starve_access Get transient graph random effects
#' @export
setGeneric(name = "tg_re",
           def = function(x) standardGeneric("tg_re")
)
#' @describeIn starve_access Set transient graph random effects
#' @export
setGeneric(name = "tg_re<-",
           def = function(x,value) standardGeneric("tg_re<-")
)

#' @describeIn starve_access Get time effects
#' @export
setGeneric(name = "time_effects",
           def = function(x) standardGeneric("time_effects")
)
#' @describeIn starve_access Set time effects
#' @export
setGeneric(name = "time_effects<-",
           def = function(x,value) standardGeneric("time_effects<-")
)

#' @describeIn starve_access Get time name used
#' @keywords internal
setGeneric(name = ".time_name",
           def = function(x) standardGeneric(".time_name")
)


#' @describeIn starve_access Get time parameters
#' @export
setGeneric(name = "time_parameters",
           def = function(x) standardGeneric("time_parameters")
)
#' @describeIn starve_access Set time parameters
#' @export
setGeneric(name = "time_parameters<-",
           def = function(x,value) standardGeneric("time_parameters<-")
)

#' @describeIn starve_access Get model computation times
#' @export
setGeneric(name = "timing",
           def = function(x) standardGeneric("timing")
)


#' @describeIn starve_access Get TMB_out
#' @export
setGeneric(name = "TMB_out",
           def = function(x) standardGeneric("TMB_out")
)
#' @describeIn starve_access Set TMB_out
setGeneric(name = "TMB_out<-",
           def = function(x,value) standardGeneric("TMB_out<-")
)

#' @describeIn starve_access Get tracing information
#' @export
setGeneric(name = "tracing",
           def = function(x) standardGeneric("tracing")
)
#' @describeIn starve_access Set tracing information
setGeneric(name = "tracing<-",
           def = function(x,value) standardGeneric("tracing<-")
)

#' @describeIn starve_access Get transient graph
setGeneric(name = "transient_graph",
           def = function(x) standardGeneric("transient_graph")
)
#' @describeIn starve_access Set transient graph
setGeneric(name = "transient_graph<-",
           def = function(x,value) standardGeneric("transient_graph<-")
)




# U



# V

#' @describeIn starve_access Get values
#' @export
setGeneric(name = "values",
           def = function(x) standardGeneric("values")
)
#' @describeIn starve_access Set values
#' @export
setGeneric(name = "values<-",
           def = function(x,value) standardGeneric("values<-")
)




# W



# X



# Y



# Z
