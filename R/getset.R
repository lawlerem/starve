#' @include classes.R
NULL



###
### Collection of all generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' @noRd
#' @export
setGeneric(name = "convergence",
           def = function(x) standardGeneric("convergence")
)

#' @noRd
#' @export
setGeneric(name = "covariance_function",
           def = function(x) standardGeneric("covariance_function")
)
#' @noRd
#' @export
setGeneric(name = "covariance_function<-",
           def = function(x,value) standardGeneric("covariance_function<-")
)




# D

#' @export
#' @noRd
setGeneric(name = "dat",
           def = function(x) standardGeneric("dat")
)
#' @export
#' @noRd
setGeneric(name = "dat<-",
           def = function(x,value) standardGeneric("dat<-")
)

#' @export
#' @noRd
setGeneric(name = "distance_units",
           def = function(x) standardGeneric("distance_units")
)
#' @export
#' @noRd
setGeneric(name = "distance_units<-",
           def = function(x,value) standardGeneric("distance_units<-")
)

#' @export
#' @noRd
setGeneric(name = "distances",
           def = function(x) standardGeneric("distances")
)
#' @export
#' @noRd
setGeneric(name = "distances<-",
           def = function(x,value) standardGeneric("distances<-")
)





# E

#' @export
#' @noRd
setGeneric(name = "edges",
           def = function(x) standardGeneric("edges")
)
#' @export
#' @noRd
setGeneric(name = "edges<-",
           def = function(x,value) standardGeneric("edges<-")
)





# F

#' @export
#' @noRd
setGeneric(name = "fixed_effects",
           def = function(x) standardGeneric("fixed_effects")
)
#' @export
#' @noRd
setGeneric(name = "fixed_effects<-",
           def = function(x,value) standardGeneric("fixed_effects<-")
)

#' @export
#' @noRd
setGeneric(name = "formula",
           def = function(x) standardGeneric("formula")
)
#' @export
#' @noRd
setGeneric(name = "formula<-",
           def = function(x,value) standardGeneric("formula<-")
)




# G

#' @export
#' @noRd
setGeneric(name = "graph",
           def = function(x) standardGeneric("graph")
)



# H

#' @export
#' @noRd
setGeneric(name = "hess_time",
           def = function(x) standardGeneric("hess_time")
)
#' @export
#' @noRd
setGeneric(name = "hess_time<-",
           def = function(x,value) standardGeneric("hess_time<-")
)




# I



# J



# K



# L

#' @export
#' @noRd
setGeneric(name = "link_function",
           def = function(x) standardGeneric("link_function")
)
#' @export
#' @noRd
setGeneric(name = "link_function<-",
           def = function(x,value) standardGeneric("link_function<-")
)




# M

#' @export
#' @noRd
setGeneric(name = "max_distance",
           def = function(x) standardGeneric("max_distance")
)
#' @export
#' @noRd
setGeneric(name = "max_distance<-",
           def = function(x,value) standardGeneric("max_distance<-")
)




# N

#' @export
#' @noRd
setGeneric(name = "n_neighbours",
           def = function(x) standardGeneric("n_neighbours")
)
#' @export
#' @noRd
setGeneric(name = "n_neighbours<-",
           def = function(x,value) standardGeneric("n_neighbours<-")
)




# O

#' @export
#' @noRd
setGeneric(name = "obj",
  def = function(x) standardGeneric("obj")
)
#' @export
#' @noRd
setGeneric(name = "obj<-",
  def = function(x,value) standardGeneric("obj<-")
)

#' @export
#' @noRd
setGeneric(name = "observations",
           def = function(x) standardGeneric("observations")
)
#' @export
#' @noRd
setGeneric(name = "observations<-",
           def = function(x,value) standardGeneric("observations<-")
)

#' @export
#' @noRd
setGeneric(name = "opt",
  def = function(x) standardGeneric("opt")
)
#' @export
#' @noRd
setGeneric(name = "opt<-",
  def = function(x,value) standardGeneric("opt<-")
)

#' @export
#' @noRd
setGeneric(name = "opt_time",
           def = function(x) standardGeneric("opt_time")
)
#' @export
#' @noRd
setGeneric(name = "opt_time<-",
           def = function(x,value) standardGeneric("opt_time<-")
)




# P

#' @export
#' @noRd
setGeneric(name = "p_far_neighbours",
           def = function(x) standardGeneric("p_far_neighbours")
)
#' @export
#' @noRd
setGeneric(name = "p_far_neighbours<-",
           def = function(x,value) standardGeneric("p_far_neighbours<-")
)

#' @export
#' @noRd
setGeneric(name = "parameter_covariance",
           def = function(x) standardGeneric("parameter_covariance")
)
#' @export
#' @noRd
setGeneric(name = "parameter_covariance<-",
           def = function(x,value) standardGeneric("parameter_covariance<-")
)

#' @export
#' @noRd
setGeneric(name = "parameter_hessian",
           def = function(x) standardGeneric("parameter_hessian")
)
#' @export
#' @noRd
setGeneric(name = "parameter_hessian<-",
           def = function(x,value) standardGeneric("parameter_hessian<-")
)

#' @export
#' @noRd
setGeneric(name = "parameters",
           def = function(x) standardGeneric("parameters")
)
#' @export
#' @noRd
setGeneric(name = "parameters<-",
           def = function(x,value) standardGeneric("parameters<-")
)

#' @export
#' @noRd
setGeneric(name = "persistent_graph",
           def = function(x) standardGeneric("persistent_graph")
)
#' @export
#' @noRd
setGeneric(name = "persistent_graph<-",
           def = function(x,value) standardGeneric("persistent_graph<-")
)

#' @export
#' @noRd
setGeneric(name = "process",
           def = function(x) standardGeneric("process")
)
#' @export
#' @noRd
setGeneric(name = "process<-",
           def = function(x,value) standardGeneric("process<-")
)




# Q



# R

#' @export
#' @noRd
setGeneric(name = "random_effects",
           def = function(x) standardGeneric("random_effects")
)
#' @export
#' @noRd
setGeneric(name = "random_effects<-",
           def = function(x,value) standardGeneric("random_effects<-")
)

#' @export
#' @noRd
setGeneric(name = "response_distribution",
           def = function(x) standardGeneric("response_distribution")
)
#' @export
#' @noRd
setGeneric(name = "response_distribution<-",
           def = function(x,value) standardGeneric("response_distribution<-")
)

#' @export
#' @noRd
setGeneric(name = "response_parameters",
           def = function(x) standardGeneric("response_parameters")
)
#' @export
#' @noRd
setGeneric(name = "response_parameters<-",
           def = function(x,value) standardGeneric("response_parameters<-")
)




# S

#' @export
#' @noRd
setGeneric(name = "sdr",
           def = function(x) standardGeneric("sdr")
)
#' @export
#' @noRd
setGeneric(name = "sdr<-",
           def = function(x,value) standardGeneric("sdr<-")
)

#' @export
#' @noRd
setGeneric(name = "sdr_time",
           def = function(x) standardGeneric("sdr_time")
)
#' @export
#' @noRd
setGeneric(name = "sdr_time<-",
           def = function(x,value) standardGeneric("sdr_time<-")
)

#' @export
#' @noRd
setGeneric(name = "settings",
           def = function(x) standardGeneric("settings")
)
#' @export
#' @noRd
setGeneric(name = "settings<-",
           def = function(x,value) standardGeneric("settings<-")
)

#' @export
#' @noRd
setGeneric(name = "spatial_parameters",
           def = function(x) standardGeneric("spatial_parameters")
)
#' @export
#' @noRd
setGeneric(name = "spatial_parameters<-",
           def = function(x,value) standardGeneric("spatial_parameters<-")
)

#' @export
#' @noRd
setGeneric(name = "symbolicAnalysis",
           def = function(x) standardGeneric("symbolicAnalysis")
)
#' @export
#' @noRd
setGeneric(name = "symbolicAnalysis<-",
           def = function(x,value) standardGeneric("symbolicAnalysis<-")
)




# T

#' @export
#' @noRd
setGeneric(name = "time_effects",
           def = function(x) standardGeneric("time_effects")
)
#' @export
#' @noRd
setGeneric(name = "time_effects<-",
           def = function(x,value) standardGeneric("time_effects<-")
)

#' @export
#' @noRd
setGeneric(name = "time_parameters",
           def = function(x) standardGeneric("time_parameters")
)
#' @export
#' @noRd
setGeneric(name = "time_parameters<-",
           def = function(x,value) standardGeneric("time_parameters<-")
)

#' @export
#' @noRd
setGeneric(name = "timing",
           def = function(x) standardGeneric("timing")
)

#' @export
#' @noRd
setGeneric(name = "TMB_out",
           def = function(x) standardGeneric("TMB_out")
)
#' @export
#' @noRd
setGeneric(name = "TMB_out<-",
           def = function(x,value) standardGeneric("TMB_out<-")
)

#' @export
#' @noRd
setGeneric(name = "tracing",
           def = function(x) standardGeneric("tracing")
)
#' @export
#' @noRd
setGeneric(name = "tracing<-",
           def = function(x,value) standardGeneric("tracing<-")
)

#' @export
#' @noRd
setGeneric(name = "transient_graph",
           def = function(x) standardGeneric("transient_graph")
)
#' @export
#' @noRd
setGeneric(name = "transient_graph<-",
           def = function(x,value) standardGeneric("transient_graph<-")
)




# U



# V



# W



# X



# Y



# Z
