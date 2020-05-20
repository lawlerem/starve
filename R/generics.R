#' @include classes.R
NULL

###
### Collection of all generics defined in this package
### Should be in alphabetical order
###



# A





# B





# C

#' @export
#' @rdname Access_staRVe
setGeneric(name = "convergence",
           def = function(x) standardGeneric("convergence")
)

#' @export
#' @rdname Access_staRVe_process_parameters
setGeneric(name = "covariance_function",
           def = function(x) standardGeneric("covariance_function")
)
#' @export
#' @rdname Access_staRVe_process_parameters
setGeneric(name = "covariance_function<-",
           def = function(x,value) standardGeneric("covariance_function<-")
)





# D

#' Create a \code{dag} object
#'
#' @export
setGeneric(name = "dag",
           def = function(x,...) standardGeneric("dag")
)

#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "data",
           def = function(x) standardGeneric("data")
)
#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "data<-",
           def = function(x,value) standardGeneric("data<-")
)

#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "distance_units",
           def = function(x) standardGeneric("distance_units")
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "distance_units<-",
           def = function(x,value) standardGeneric("distance_units<-")
)

#' @export
#' @rdname Access_dag
setGeneric(name = "distances",
           def = function(x) standardGeneric("distances")
)
#' @export
#' @rdname Access_dag
setGeneric(name = "distances<-",
           def = function(x,value) standardGeneric("distances<-")
)





# E

#' @export
#' @rdname Access_dag
setGeneric(name = "edges",
           def = function(x) standardGeneric("edges")
)
#' @export
#' @rdname Access_dag
setGeneric(name = "edges<-",
           def = function(x,value) standardGeneric("edges<-")
)





# F

#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "fixed_effects",
           def = function(x) standardGeneric("fixed_effects")
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "fixed_effects<-",
           def = function(x,value) standardGeneric("fixed_effects<-")
)

#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "formula",
           def = function(x) standardGeneric("formula")
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "formula<-",
           def = function(x,value) standardGeneric("formula<-")
)





# G

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_geo_vars",
           def = function(x,var,sf_obj,get_sd) standardGeneric("get_geo_vars")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_obs",
           def = function(x,sf_obj) standardGeneric("get_obs")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_observation",
           def = function(x,sf_obj) standardGeneric("get_observation")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_parameters",
           def = function(x) standardGeneric("get_parameters")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_pars",
           def = function(x) standardGeneric("get_pars")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_proc",
           def = function(x,sf_obj) standardGeneric("get_proc")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_process",
           def = function(x,sf_obj) standardGeneric("get_process")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_resp",
           def = function(x,sf_obj) standardGeneric("get_resp")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "get_working_pars",
           def = function(x) standardGeneric("get_working_pars")
)





# H

#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "hess_time",
           def = function(x) standardGeneric("hess_time")
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "hess_time<-",
           def = function(x,value) standardGeneric("hess_time<-")
)





# I

#' Increase indices by one to translate from C to R.
#'
#' @export
setGeneric(name = "idxC_to_R",
def = function(x) standardGeneric("idxC_to_R")
)
#' Reduce indices by one to translate from R to C.
#'
#' @export
setGeneric(name = "idxR_to_C",
           def = function(x) standardGeneric("idxR_to_C")
)




# J





# K





# L

#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "link_function",
           def = function(x) standardGeneric("link_function")
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "link_function<-",
           def = function(x,value) standardGeneric("link_function<-")
)





# M

#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "max_distance",
           def = function(x) standardGeneric("max_distance")
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "max_distance<-",
           def = function(x,value) standardGeneric("max_distance<-")
)





# N

#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "n_neighbours",
           def = function(x) standardGeneric("n_neighbours")
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "n_neighbours<-",
           def = function(x,value) standardGeneric("n_neighbours<-")
)





# O

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "obj",
  def = function(x) standardGeneric("obj")
)
#' @export
#' @rdname Access_TMB_out
setGeneric(name = "obj<-",
  def = function(x,value) standardGeneric("obj<-")
)

#' @export
#' @rdname Access_staRVe_model
setGeneric(name = "observations",
           def = function(x) standardGeneric("observations")
)
#' @export
#' @rdname Access_staRVe_model
setGeneric(name = "observations<-",
           def = function(x,value) standardGeneric("observations<-")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "opt",
  def = function(x) standardGeneric("opt")
)
#' @export
#' @rdname Access_TMB_out
setGeneric(name = "opt<-",
  def = function(x,value) standardGeneric("opt<-")
)

#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "opt_time",
           def = function(x) standardGeneric("opt_time")
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "opt_time<-",
           def = function(x,value) standardGeneric("opt_time<-")
)





# P

#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "p_far_neighbours",
           def = function(x) standardGeneric("p_far_neighbours")
)
#' @export
#' @rdname Access_staRVe_settings
setGeneric(name = "p_far_neighbours<-",
           def = function(x,value) standardGeneric("p_far_neighbours<-")
)

#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_covariance",
           def = function(x) standardGeneric("parameter_covariance")
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_covariance<-",
           def = function(x,value) standardGeneric("parameter_covariance<-")
)

#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_hessian",
           def = function(x) standardGeneric("parameter_hessian")
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "parameter_hessian<-",
           def = function(x,value) standardGeneric("parameter_hessian<-")
)

#' @export
#' @rdname Access_staRVe_process
setGeneric(name = "parameters",
           def = function(x) standardGeneric("parameters")
)
#' @export
#' @rdname Access_staRVe_process
setGeneric(name = "parameters<-",
           def = function(x,value) standardGeneric("parameters<-")
)

#' @export
#' @rdname Access_staRVe_process
setGeneric(name = "persistent_graph",
           def = function(x) standardGeneric("persistent_graph")
)
#' @export
#' @rdname Access_staRVe_process
setGeneric(name = "persistent_graph<-",
           def = function(x,value) standardGeneric("persistent_graph<-")
)

#' Predict from a \code{staRVe} object.
#'
#' @param x An object of class \code{staRVe}.
#' @param locations Either an object of class \code{sf} containing point geometries,
#'  or an object of class \code{RasterLayer}. This object should not have any
#'  time information, as predictions will be made at each location at every time.
#'  If a \code{RasterLayer} object, predictions will be made for all raster cells
#'  whose value are not NA. If the raster has no values, then predictions will
#'  be made at every cell. Raster predictions are made at the midpoint of each cell.
#' @param covariates Either a data.frame of class \code{sf} or a list of \code{Raster*}
#'  objects, depending on the input type of \code{locations}. If the model has
#'  no covariates, then nothing needs to be supplied.
#'
#'  If \code{locations} is of class \code{sf} with point geometries, then
#'  \code{covariates} should also be of class \code{sf}. The data.frame should contain
#'  columns for each of the covariates, a column for the time index, and a column
#'  of point geometries. For each time unit and prediction point, there should
#'  be a row in the \code{covariates} data.frame, but the rows do not need to be
#'  in the same order as \code{locations}.
#'
#'  If \code{locations} is of class \code{RasterLayer}, then \code{covariates}
#'  should be a list of \code{Raster*} objects. Each \code{Raster*} object should
#'  contain data for one covariate, should have one layer for each time unit,
#'  and should have the same raster geometry as the \code{locations} object. The
#'  layer names of each raster layer should be of the form \code{T####}, where
#'  \code{####} gives the specific time index.
#' @param time What time indices should predictions be made for? If set to "model",
#'  predictions are made for every time present in the model. The supplied time
#'  indices must be a subset of those in the model.
#'
#' @return Either a \code{sf} object or a list of \code{Raster*} objects,
#'  containing predictions (with standard errors) for the spatial field, the linear
#'  predictor on the link scale, and the response.
#'  The return type is the same as the type of input for \code{locations}.
#'
#' @export
setGeneric(name = "predict_staRVe",
           def = function(x,locations,...) standardGeneric("predict_staRVe")
)

#' @export
#' @rdname Access_staRVe_model
setGeneric(name = "process",
           def = function(x) standardGeneric("process")
)
#' @export
#' @rdname Access_staRVe_model
setGeneric(name = "process<-",
           def = function(x,value) standardGeneric("process<-")
)





# Q





# R

#' @export
#' @rdname Access_staRVe_process
setGeneric(name = "random_effects",
           def = function(x) standardGeneric("random_effects")
)
#' @export
#' @rdname Access_staRVe_process
setGeneric(name = "random_effects<-",
           def = function(x,value) standardGeneric("random_effects<-")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "report",
           def = function(x) standardGeneric("report")
)

#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_distribution",
           def = function(x) standardGeneric("response_distribution")
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_distribution<-",
           def = function(x,value) standardGeneric("response_distribution<-")
)

#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_parameters",
           def = function(x) standardGeneric("response_parameters")
)
#' @export
#' @rdname Access_staRVe_observation_parameters
setGeneric(name = "response_parameters<-",
           def = function(x,value) standardGeneric("response_parameters<-")
)





# S

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "sdr",
           def = function(x) standardGeneric("sdr")
)
#' @export
#' @rdname Access_TMB_out
setGeneric(name = "sdr<-",
           def = function(x,value) standardGeneric("sdr<-")
)

#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "sdr_time",
           def = function(x) standardGeneric("sdr_time")
)
#' @export
#' @rdname Access_staRVe_tracing
setGeneric(name = "sdr_time<-",
           def = function(x,value) standardGeneric("sdr_time<-")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "sdreport",
           def = function(x) standardGeneric("sdreport")
)

#' @export
#' @rdname Access_staRVe_model
setGeneric(name = "settings",
           def = function(x) standardGeneric("settings")
)
#' @export
#' @rdname Access_staRVe_model
setGeneric(name = "settings<-",
           def = function(x,value) standardGeneric("settings<-")
)

#' @export
#' @rdname Access_staRVe_process_parameters
setGeneric(name = "spatial_parameters",
           def = function(x) standardGeneric("spatial_parameters")
)
#' @export
#' @rdname Access_staRVe_process_parameters
setGeneric(name = "spatial_parameters<-",
           def = function(x,value) standardGeneric("spatial_parameters<-")
)

#' Create a \code{staRVe_fit} object
#'
#' @export
setGeneric(name = "staRVe_fit",
           def = function(x,...) standardGeneric("staRVe_fit")
)

#' Create a \code{staRVe_model} object
#'
#' @export
setGeneric(name = "staRVe_model",
           def = function(x,...) standardGeneric("staRVe_model")
)

#' Create a \code{staRVe_observation_parameters} object
#'
#' @export
setGeneric(name = "staRVe_observation_parameters",
           def = function(x,...) standardGeneric("staRVe_observation_parameters")
)

#' Create a \code{staRVe_observations} object
#'
#' @export
setGeneric(name = "staRVe_observations",
           def = function(x,...) standardGeneric("staRVe_observations")
)

#' Create a \code{staRVe_process} object
#'
#' @export
setGeneric(name = "staRVe_process",
           def = function(x,...) standardGeneric("staRVe_process")
)

#' Create a \code{staRVe_process_parameters} object
#'
#' @export
setGeneric(name = "staRVe_process_parameters",
           def = function(x,...) standardGeneric("staRVe_process_parameters")
)

#' Create a \code{staRVe_settings} object
#'
#' @export
setGeneric(name = "staRVe_settings",
           def = function(x,...) standardGeneric("staRVe_settings")
)

#' Create a \code{staRVe_tracing} object
#'
#' @export
setGeneric(name = "staRVe_tracing",
           def = function(x,...) standardGeneric("staRVe_tracing")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "symbolicAnalysis",
           def = function(x) standardGeneric("symbolicAnalysis")
)
#' @export
#' @rdname Access_TMB_out
setGeneric(name = "symbolicAnalysis<-",
           def = function(x,value) standardGeneric("symbolicAnalysis<-")
)





# T

#' @export
#' @rdname Access_staRVe_process_parameters
setGeneric(name = "time_parameters",
           def = function(x) standardGeneric("time_parameters")
)
#' @export
#' @rdname Access_staRVe_process_parameters
setGeneric(name = "time_parameters<-",
           def = function(x,value) standardGeneric("time_parameters<-")
)

#' @export
#' @rdname Access_TMB_out
setGeneric(name = "TMB_in",
           def = function(x) standardGeneric("TMB_in")
)

#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "TMB_out",
           def = function(x) standardGeneric("TMB_out")
)
#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "TMB_out<-",
           def = function(x,value) standardGeneric("TMB_out<-")
)

#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "tracing",
           def = function(x) standardGeneric("tracing")
)
#' @export
#' @rdname Access_staRVe_fit
setGeneric(name = "tracing<-",
           def = function(x,value) standardGeneric("tracing<-")
)

#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "transient_graph",
           def = function(x) standardGeneric("transient_graph")
)
#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "transient_graph<-",
           def = function(x,value) standardGeneric("transient_graph<-")
)




# U

#' Update a \code{staRVe_model} object from a model fit.
#'
#' @export
setGeneric(name = "update_staRVe_model",
           def = function(x,y) standardGeneric("update_staRVe_model")
)





# V





# W





# X





# Y





# Z
