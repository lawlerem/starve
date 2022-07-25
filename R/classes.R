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
#'   \item long_stars
#'   \item staRVe_settings
#'   \item staRVe_process_parameters
#'   \item staRVe_process
#'   \item staRVe_observation_parameters
#'   \item staRVe_observations
#'   \item staRVe_model
#'   \item staRVe_tracing
#'   \item TMB_out
#'   \item staRVe_model_fit
#'   \item staRVe_parameters
#' }
#'
#' @family staRVe_classes
#'
#' @name staRVe_classes
#'
#' @keywords internal
NULL

# All stars slots should have dimensions in order: space, time, variable

#' Constructors for the staRVe package
#'
#' @param .Object The object to create
#'
#' @keywords internal
#'
#' @name  staRVe-construct
NULL


setOldClass("proc_time")
setOldClass("sdreport")
setOldClass("sf")
setOldClass("stars")

#' An S4 class to hold a directed acyclic graph with distances.
#'
#' @slot edges A list whose elements consist of a "to" vector and a
#'   "from" vector. There is a directed edge in the graph from each vertex in
#'   "from" to each vertex in "to". If there are multiple vertices in "to" then
#'   there is an undirected edge between all vertices in "to" (forming a Kn graph).
#' @slot distances A list whose elements consist of a distance matrix for the edges
#'   in the correponding entry of edges. The rows/columns of the distance matrix
#'   are in the same order as the vertices in that edge list, starting with the
#'   vertices in "to" and then the vertices in "from".
#' @slot distance_units The units used for distance calculation.  Must be compatible
#'   with units::set_units.
#'
#' @aliases dag
#'
#' @seealso construct_dag Functions used to construct direct acyclic graphs
#'
#' @family staRVe_classes
setClass(
  Class = "dag",
  slots = c(
    edges = "list",
    distances = "list",
    distance_units = "character"
  )
)

#' An S4 class to hold predictions from a staRVe model.
#'
#' @details The \code{predictions} slot will have at least 1 dimension, the first
#'   of which must be named \code{idx} and is identical to the row dimension of
#'   \code{locations}.
#'
#' @slot predictions A stars object.
#' @slot locations An sf object with at least a time column and point geometries.
#'
#' @aliases long_stars
#'
#' @family staRVe_classes
setClass(
  Class = "long_stars",
  slots = c(
    values = "stars",
    locations = "sf"
  )
)


#' An S4 class to hold extra settings for a staRVe model
#'
#' @slot formula A formula
#' @slot n_neighbours The number of parents for each node in the graph.
#' @slot distance_units The units used for distance calculation.  Must be compatible
#'   with units::set_units.
#' @slot max_distance The maximum allowable distance for edges in the transient graph.
#'
#' @aliases staRVe_settings
#'
#' @family staRVe_classes
#'
#' @keywords internal
setClass(
  Class = "staRVe_settings",
  slots = c(
    formula = "formula",
    n_neighbours = "numeric",
    distance_units = "character",
    max_distance = "numeric"
  )
)


#' An S4 class to hold process parameters for a staRVe model.
#'
#' @slot covariance_function The covariance function(s), must be one from
#'   get_staRVe_distributions("covariance").
#' @slot spatial_parameters A list containing spatial parameters for each response variable.
#' @slot time_parameters A list containing time parameters for each response variable.
#'
#' @family staRVe_classes
#'
#' @aliases staRVe_process_parameters
#'
#' @keywords internal
setClass(
  Class = "staRVe_process_parameters",
  slots = c(
    covariance_function = "character",
    spatial_parameters = "list",
    time_parameters = "list"
  )
)

#' An S4 class to hold the process information for a staRVe model.
#'
#' @slot time_effects A stars object containing temporal random effects
#' @slot random_effects A stars object containing spatio-temporal random effects.
#' @slot persistent_graph A dag object describing the dependence graph of the process.
#' @slot parameters An object of class staRVe_process_parameters.
#'
#' @seealso prepare_staRVe_process
#'
#' @family staRVe_classes
#'
#' @aliases staRVe_process
#'
#' @keywords internal
setClass(
  Class = "staRVe_process",
  slots = c(
    time_effects = "stars",
    pg_re = "stars",
    tg_re = "long_stars",
    persistent_graph = "dag",
    transient_graph = "dag",
    parameters = "staRVe_process_parameters"
  )
)

#' An S4 class to hold observation parameters for a staRVe model.
#'
#' @slot response_distribution The response distribution(s), must be one from
#'   get_staRVe_distributions("distribution").
#' @slot response_parameters A list containing a data.frame of response distribution
#'   parameters for each response variable.
#' @slot link_function The link function(s), must be one from
#'   get_staRVe_distributions("link").
#' @slot fixed_effects A list containing a data.frame of fixed effect parameters
#'   for each response variable.
#'
#' @family staRVe_classes
#'
#' @aliases staRVe_observation_parameters
#'
#' @keywords internal
setClass(
  Class = "staRVe_observation_parameters",
  slots = c(
    response_distribution = "character",
    response_parameters = "list",
    link_function = "character",
    fixed_effects = "list"
  )
)

#' An S4 class to hold the observation information for a staRVe model.
#'
#' @slot data_predictions A long_stars object for the data.
#' @slot transient_graph A dag object describing the dependence of the data on
#'   the process.
#' @slot parameters An object of class staRVe_observation_parameters.
#'
#' @seealso prepare_staRVe_observations
#'
#' @family staRVe_classes
#'
#' @aliases staRVe_observations
#'
#' @keywords internal
setClass(
  Class = "staRVe_observations",
  slots = c(
    data_predictions = "long_stars",
    parameters = "staRVe_observation_parameters"
  )
)


#' An S4 class describing a staRVe model.
#'
#' @slot process A staRVe_process object.
#' @slot observations A staRVe_observations object.
#' @slot settings A staRVe_settings object.
#'
#' @aliases staRVe_model
#'
#' @seealso prepare_staRVe_model
#'
#' @family staRVe_classes
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
#' @slot opt_time A proc_time object. Time elapsed while computing the ML estimates.
#' @slot hess_time A proc_time object. Time elapsed while computing the hessian matrix.
#' @slot sdr_time A proc_time object. Time elapsed while computing standard errors.
#' @slot parameter_hessian The hessian matrix for parameter estimates.
#' @slot parameter_covariance The covariance matrix for parameter estimates.
#'
#' @aliases staRVe_tracing
#'
#' @family staRVe_classes
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
#' @family staRVe_classes
#'
#' @keywords internal
setClass(
  Class = "TMB_out",
  slots = c(
    obj = "list",
    opt = "list",
    sdr = "sdreport"
  )
)

#' An extension of the staRVe_model class to hold optimization information
#'
#' @slot tracing A staRVe_tracing object
#' @slot TMB_out A TMB_out object (for internal use)
#'
#' @aliases staRVe_model_fit
#'
#' @seealso staRVe_model, staRVe_tracing, TMB_out
#'
#' @family staRVe_classes
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
#'
#' @seealso staRVe_process_parameters, staRVe_observation_paraeters, staRVe_model
#'
#' @family staRVe_classes
#'
#' @keywords internal
setClass(
  Class = "staRVe_parameters",
  contains = c("staRVe_process_parameters",
               "staRVe_observation_parameters")
)
