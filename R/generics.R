#' @include classes.R
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

#' @export
#' @rdname staRVe_generics
setGeneric(name = "convergence",
           def = function(x) standardGeneric("convergence")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "covariance_function",
           def = function(x) standardGeneric("covariance_function")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "covariance_function<-",
           def = function(x,value) standardGeneric("covariance_function<-")
)





# D

#' @export
#' @rdname staRVe_generics
setGeneric(name = "dag",
           def = function(x,...) standardGeneric("dag")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "dat",
           def = function(x) standardGeneric("dat")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "dat<-",
           def = function(x,value) standardGeneric("dat<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "distance_units",
           def = function(x) standardGeneric("distance_units")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "distance_units<-",
           def = function(x,value) standardGeneric("distance_units<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "distances",
           def = function(x) standardGeneric("distances")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "distances<-",
           def = function(x,value) standardGeneric("distances<-")
)





# E

#' @export
#' @rdname staRVe_generics
setGeneric(name = "edges",
           def = function(x) standardGeneric("edges")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "edges<-",
           def = function(x,value) standardGeneric("edges<-")
)





# F

#' @export
#' @rdname staRVe_generics
setGeneric(name = "fixed_effects",
           def = function(x) standardGeneric("fixed_effects")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "fixed_effects<-",
           def = function(x,value) standardGeneric("fixed_effects<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "formula",
           def = function(x) standardGeneric("formula")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "formula<-",
           def = function(x,value) standardGeneric("formula<-")
)





# G

#' @export
#' @rdname staRVe_generics
setGeneric(name = "graph",
           def = function(x) standardGeneric("graph")
)





# H

#' @export
#' @rdname staRVe_generics
setGeneric(name = "hess_time",
           def = function(x) standardGeneric("hess_time")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "hess_time<-",
           def = function(x,value) standardGeneric("hess_time<-")
)





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

#' @export
#' @rdname staRVe_generics
setGeneric(name = "link_function",
           def = function(x) standardGeneric("link_function")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "link_function<-",
           def = function(x,value) standardGeneric("link_function<-")
)





# M

#' @export
#' @rdname staRVe_generics
setGeneric(name = "max_distance",
           def = function(x) standardGeneric("max_distance")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "max_distance<-",
           def = function(x,value) standardGeneric("max_distance<-")
)





# N

#' @export
#' @rdname staRVe_generics
setGeneric(name = "n_neighbours",
           def = function(x) standardGeneric("n_neighbours")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "n_neighbours<-",
           def = function(x,value) standardGeneric("n_neighbours<-")
)





# O

#' @export
#' @rdname staRVe_generics
setGeneric(name = "obj",
  def = function(x) standardGeneric("obj")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "obj<-",
  def = function(x,value) standardGeneric("obj<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "observations",
           def = function(x) standardGeneric("observations")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "observations<-",
           def = function(x,value) standardGeneric("observations<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "opt",
  def = function(x) standardGeneric("opt")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "opt<-",
  def = function(x,value) standardGeneric("opt<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "opt_time",
           def = function(x) standardGeneric("opt_time")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "opt_time<-",
           def = function(x,value) standardGeneric("opt_time<-")
)





# P

#' @export
#' @rdname staRVe_generics
setGeneric(name = "p_far_neighbours",
           def = function(x) standardGeneric("p_far_neighbours")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "p_far_neighbours<-",
           def = function(x,value) standardGeneric("p_far_neighbours<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "parameter_covariance",
           def = function(x) standardGeneric("parameter_covariance")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "parameter_covariance<-",
           def = function(x,value) standardGeneric("parameter_covariance<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "parameter_hessian",
           def = function(x) standardGeneric("parameter_hessian")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "parameter_hessian<-",
           def = function(x,value) standardGeneric("parameter_hessian<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "parameters",
           def = function(x) standardGeneric("parameters")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "parameters<-",
           def = function(x,value) standardGeneric("parameters<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "persistent_graph",
           def = function(x) standardGeneric("persistent_graph")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "persistent_graph<-",
           def = function(x,value) standardGeneric("persistent_graph<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "process",
           def = function(x) standardGeneric("process")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "process<-",
           def = function(x,value) standardGeneric("process<-")
)





# Q





# R

#' @export
#' @rdname staRVe_generics
setGeneric(name = "random_effects",
           def = function(x) standardGeneric("random_effects")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "random_effects<-",
           def = function(x,value) standardGeneric("random_effects<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "report",
           def = function(x) standardGeneric("report")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "response_distribution",
           def = function(x) standardGeneric("response_distribution")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "response_distribution<-",
           def = function(x,value) standardGeneric("response_distribution<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "response_parameters",
           def = function(x) standardGeneric("response_parameters")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "response_parameters<-",
           def = function(x,value) standardGeneric("response_parameters<-")
)





# S

#' @export
#' @rdname staRVe_generics
setGeneric(name = "sdr",
           def = function(x) standardGeneric("sdr")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "sdr<-",
           def = function(x,value) standardGeneric("sdr<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "sdr_time",
           def = function(x) standardGeneric("sdr_time")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "sdr_time<-",
           def = function(x,value) standardGeneric("sdr_time<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "sdreport",
           def = function(x) standardGeneric("sdreport")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "settings",
           def = function(x) standardGeneric("settings")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "settings<-",
           def = function(x,value) standardGeneric("settings<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "spatial_parameters",
           def = function(x) standardGeneric("spatial_parameters")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "spatial_parameters<-",
           def = function(x,value) standardGeneric("spatial_parameters<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_fit",
           def = function(x,...) standardGeneric("staRVe_fit")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_model",
           def = function(x,...) standardGeneric("staRVe_model")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_observation_parameters",
           def = function(x,...) standardGeneric("staRVe_observation_parameters")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_observations",
           def = function(x,...) standardGeneric("staRVe_observations")
)


#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_predict",
           def = function(x,locations,...) standardGeneric("staRVe_predict")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_process",
           def = function(x,...) standardGeneric("staRVe_process")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_process_parameters",
           def = function(x,...) standardGeneric("staRVe_process_parameters")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_settings",
           def = function(x,...) standardGeneric("staRVe_settings")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_simulate",
           def = function(model,...) standardGeneric("staRVe_simulate")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "staRVe_tracing",
           def = function(x,...) standardGeneric("staRVe_tracing")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "symbolicAnalysis",
           def = function(x) standardGeneric("symbolicAnalysis")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "symbolicAnalysis<-",
           def = function(x,value) standardGeneric("symbolicAnalysis<-")
)





# T

#' @export
#' @rdname staRVe_generics
setGeneric(name = "time_effects",
           def = function(x) standardGeneric("time_effects")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "time_effects<-",
           def = function(x,value) standardGeneric("time_effects<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "time_parameters",
           def = function(x) standardGeneric("time_parameters")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "time_parameters<-",
           def = function(x,value) standardGeneric("time_parameters<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "timing",
           def = function(x) standardGeneric("timing")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "TMB_in",
           def = function(x) standardGeneric("TMB_in")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "TMB_out",
           def = function(x) standardGeneric("TMB_out")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "TMB_out<-",
           def = function(x,value) standardGeneric("TMB_out<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "tracing",
           def = function(x) standardGeneric("tracing")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "tracing<-",
           def = function(x,value) standardGeneric("tracing<-")
)

#' @export
#' @rdname staRVe_generics
setGeneric(name = "transient_graph",
           def = function(x) standardGeneric("transient_graph")
)
#' @export
#' @rdname staRVe_generics
setGeneric(name = "transient_graph<-",
           def = function(x,value) standardGeneric("transient_graph<-")
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
