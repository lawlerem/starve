#' A list of classes in the staRVe package.
#'
#' To see documentation for a particular class, load the package and run e.g.
#'   class?staRVe_model. To see a list of all methods available for a specific class,
#'   see the help page or run \code{methods(class="staRVe_model")}.
#'
#'
#' @section Classes:
#' \itemize{
#'   \item dag
#'   \item staRVe_process_parameters
#'   \item staRVe_process
#'   \item staRVe_predictions
#'   \item staRVe_observation_parameters
#'   \item staRVe_observations
#'   \item staRVe_settings
#'   \item staRVe_model
#'   \item staRVe_tracing
#'   \item TMB_out
#'   \item staRVe_model_fit
#'   \item staRVe_parameters
#' }
#'
#' @name staRVe_classes
NULL

#' Constructors for the staRVe package
#'
#' @param .Object The object to create
#'
#' @name  staRVe-construct
NULL


setOldClass("proc_time")
setOldClass("sdreport")
setOldClass("sf")
setOldClass("stars")

#' An S4 class to hold a directed acyclic graph with distances.
#'
#' @slot edges A list of integer vector, indexing the parents of each node.
#' @slot distances A list of matrices containing the distances between
#'   nodes and parents.
#' @slot distance_units The units used for distance calculation.  Must be compatible
#'   with units::set_units.
setClass(
  Class = "dag",
  slots = c(
    edges = "list",
    distances = "list",
    distance_units = "character"
  )
)

#' An S4 class to hold process parameters for a staRVe model.
#'
#' @slot covariance_function The covariance function, must be one given by
#'   get_staRVe_distributions("covariance").
#' @slot spatial_parameters A data.frame containing spatial parameters.
#' @slot time_parameters A data.frame containing time parameters.
setClass(
  Class = "staRVe_process_parameters",
  slots = c(
    covariance_function = "character",
    spatial_parameters = "data.frame",
    time_parameters = "data.frame"
  )
)

#' An S4 class to hold the process information for a staRVe model.
#'
#' @slot time_effects A stars object containing temporal random effects
#' @slot random_effects A stars object containing spatio-temporal random effects.
#' @slot persistent_graph A dag object describing the dependence graph of the process.
#' @slot parameters An object of class staRVe_process_parameters.
setClass(
  Class = "staRVe_process",
  slots = c(
    time_effects = "stars",
    random_effects = "stars",
    persistent_graph = "dag",
    parameters = "staRVe_process_parameters"
  ),
)

#' An S4 class to hold predictions from a staRVe model.
#'
#' @slot predictions A stars object
#' @slot locations An sf object with covariates, a time column, and point geometries.
setClass(
  Class = "staRVe_predictions",
  slots = c(
    predictions = "stars",
    locations = "sf"
  )
)

#' An S4 class to hold observation parameters for a staRVe model.
#'
#' @slot response_distribution The response distribution, must be one given by
#'   get_staRVe_distributions("distribution").
#' @slot response_parameters A data.frame containing parameters for the response
#'   distribution.
#' @slot link_function The link function, must be one given by
#'   get_staRVe_distributions("link").
#' @slot fixed_effects A data.frame containing the fixed effect parameters.
setClass(
  Class = "staRVe_observation_parameters",
  slots = c(
    response_distribution = "character",
    response_parameters = "data.frame",
    link_function = "character",
    fixed_effects = "data.frame"
  )
)

#' An S4 class to hold the observation information for a staRVe model.
#'
#' @slot data_predictions A staRVe_predictions object for the data.
#' @slot transient_graph A dag object describing the dependence of the data on
#'   the process.
#' @slot parameters An object of class staRVe_observation_parameters.
setClass(
  Class = "staRVe_observations",
  slots = c(
    data_predictions = "staRVe_predictions",
    transient_graph = "dag",
    parameters = "staRVe_observation_parameters"
  )
)

#' An S4 class to hold extra settings for a staRVe model
#'
#' @slot formula A formula
#' @slot n_neighbours The number of parents for each node in the graph.
#' @slot p_far_neighbours The proportion of parents randomly chosen for each node.
#' @slot distance_units The units used for distance calculation.  Must be compatible
#'   with units::set_units.
#' @slot max_distance The maximum distance to look for parents. Does not affect
#'   parents coming from p_far_neighbours.
#' @slot obs_dag_method Which method should be used for constructing the transient graph?
#' @slot extras Any additional settings that aren't essential for the model. E.g.
#'   could hold an inla.mesh object.
#'
#' @aliases staRVe_settings
setClass(
  Class = "staRVe_settings",
  slots = c(
    formula = "formula",
    n_neighbours = "numeric",
    p_far_neighbours = "numeric",
    distance_units = "character",
    max_distance = "numeric",
    obs_dag_method = "character",
    extras = "list"
  )
)

#' An S4 class describing a staRVe model.
#'
#' @slot process A staRVe_process object.
#' @slot observations A staRVe_observations object.
#' @slot settings A staRVe_settings object.
#'
#' @aliases staRVe_model
setClass(
  Class = "staRVe_model",
  slots = c(
    process = "staRVe_process",
    observations = "staRVe_observations",
    settings = "staRVe_settings"
  )
)

#' An S4 class to hold optimization tracing for a staRVe model
#'
#' @slot opt_time A proc_time object. Time used to compute the ML estimates.
#' @slot hess_time A proc_time object. Time used to compute the hessian matrix.
#' @slot sdr_time A proc_time object. Time used to compute standard errors.
#' @slot parameter_hessian The hessian matrix for parameter estimates.
#' @slot parameter_covariance The covariance matrix for parameter estimates.
#'
#' @aliases staRVe_tracing
setClass(
  Class = "staRVe_tracing",
  slots = c(
    opt_time = "proc_time",
    hess_time = "proc_time",
    sdr_time = "proc_time",
    parameter_hessian = "matrix",
    parameter_covariance = "matrix"
  )
)

#' An S4 class to hold TMB-related objects
#'
#' @slot obj The output of TMB::MakeADFun.
#' @slot opt The output of nlminb.
#' @slot sdr The output of TMB::sdreport.
#'
#' @param x An object
#' @param object An object
setClass(
  Class = "TMB_out",
  slots = c(
    obj = "list",
    opt = "list",
    sdr = "sdreport"
  )
)

#' An S4 class to hold an optimized staRVe model, extends the staRVe_model class
#'
#' @slot tracing A staRVe_tracing object.
#' @slot TMB_out A TMB_out object.
#'
#' @aliases staRVe_model_fit
setClass(
  Class = "staRVe_model_fit",
  slots = c(
    tracing = "staRVe_tracing",
    TMB_out = "TMB_out"
  ),
  contains = c("staRVe_model")
)





######################
### Helper Classes ###
######################

#' An S4 class to collect process and observations parameters.
setClass(
  Class = "staRVe_parameters",
  contains = c("staRVe_process_parameters",
               "staRVe_observation_parameters")
)
