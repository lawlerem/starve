#' A list of classes in the starve package.
#'
#' To see documentation for a particular class, load the package and run e.g.
#'   class?starve. To see a list of all methods available for a specific class,
#'   see the help page for that class or run e.g. \code{methods(class="starve")}.
#'
#' @section Classes:
#' \itemize{
#'   \item dag
#'   \item long_stars
#'   \item settings
#'   \item process_parameters
#'   \item process
#'   \item observation_parameters
#'   \item observations
#'   \item tracing
#'   \item TMB_out
#'   \item starve
#'   \item parameters
#' }
#'
#' @family starve_classes
#'
#' @name starve_classes
#'
#' @noRd
NULL

# All stars slots should have dimensions in order: space, time, variable


setOldClass("formula")
setOldClass("proc_time")
setOldClass("sdreport")
setOldClass("sf")
setOldClass("stars")

#' An S4 class to hold a directed acyclic graph with distances.
#'
#' @slot edges A list whose elements consist of a "to" vector and a
#'   "from" vector. There is a directed edge in the graph from each vertex in
#'   "from" to each vertex in "to". If there are multiple vertices in "to" then
#'   there are undirected edges between all vertices in "to" (forming a Kn graph).
#' @slot distances A list whose elements consist of a distance matrix for the edges
#'   in the correponding entry of edges. The rows/columns of the distance matrix
#'   are in the same order as the vertices in that edge list, starting with the
#'   vertices in "to" and then the vertices in "from".
#' @slot distance_units The units used for distance calculation.  Must be compatible
#'   with units::set_units.
#'
#' @rdname dag_class
#'
#' @seealso construct_graph Functions used to create graphs.
#'
#' @family starve_classes
setClass(
  Class = "dag",
  slots = c(
    edges = "list",
    distances = "list",
    distance_units = "character"
  )
)

#' An S4 class to hold a multi-dimensional array and a data.frame with a
#'   many-to-one relationship.
#'
#' @details The \code{predictions} slot will have at least 1 dimension, the first
#'   of which must be named \code{idx} and is identical to the row dimension of
#'   \code{locations}.
#'
#' @slot predictions A stars object.
#' @slot locations An sf object with at least a time column and point geometries.
#'
#' @rdname long_stars_class
#'
#' @family starve_classes
setClass(
  Class = "long_stars",
  slots = c(
    values = "stars",
    locations = "sf"
  )
)


#' An S4 class to hold settings for a starve model
#'
#' @slot formula A formula
#' @slot n_neighbours The number of parents for each node in the graph.
#' @slot distance_units The units used for distance calculation.  Must be compatible
#'   with units::set_units.
#' @slot max_distance The maximum allowable distance for edges in the transient graph.
#'
#' @rdname settings_class
#'
#' @family starve_classes
#'
#' @keywords internal
setClass(
  Class = "settings",
  slots = c(
    formula = "formula",
    n_neighbours = "numeric",
    distance_units = "character",
    max_distance = "numeric"
  )
)


#' An S4 class to hold process parameters for a starve model.
#'
#' @slot covariance_function The covariance function(s), must be one from
#'   get_starve_distributions("covariance").
#' @slot space_parameters A list containing spatial parameters for each response variable.
#' @slot time_parameters A list containing time parameters for each response variable.
#'
#' @family starve_classes
#'
#' @rdname process_parameters_class
#'
#' @keywords internal
setClass(
  Class = "process_parameters",
  slots = c(
    covariance_function = "character",
    space_parameters = "list",
    time_parameters = "list"
  )
)

#' An S4 class to hold the process information for a starve model.
#'
#' @slot time_effects A stars object containing temporal random effects
#' @slot random_effects A stars object containing spatio-temporal random effects.
#' @slot persistent_graph A dag object describing the dependence graph of the process.
#' @slot parameters An object of class process_parameters.
#'
#' @seealso strv_prepare
#'
#' @family starve_classes
#'
#' @rdname process_class
#'
#' @keywords internal
setClass(
  Class = "process",
  slots = c(
    time_effects = "stars",
    pg_re = "stars",
    tg_re = "long_stars",
    persistent_graph = "dag",
    transient_graph = "dag",
    parameters = "process_parameters"
  )
)

#' An S4 class to hold observation parameters for a starve model.
#'
#' @slot response_distribution The response distribution(s), must be one from
#'   get_starve_distributions("distribution").
#' @slot response_parameters A list containing a data.frame of response distribution
#'   parameters for each response variable.
#' @slot link_function The link function(s), must be one from
#'   get_starve_distributions("link").
#' @slot fixed_effects A list containing a data.frame of fixed effect parameters
#'   for each response variable.
#'
#' @family starve_classes
#'
#' @rdname observation_parameters_class
#'
#' @keywords internal
setClass(
  Class = "observation_parameters",
  slots = c(
    response_distribution = "character",
    response_parameters = "list",
    link_function = "character",
    fixed_effects = "list"
  )
)

#' An S4 class to hold the observation information for a starve model.
#'
#' @slot data_predictions A long_stars object for the data.
#' @slot transient_graph A dag object describing the dependence of the data on
#'   the process.
#' @slot parameters An object of class observation_parameters.
#'
#' @seealso strv_prepare
#'
#' @family starve_classes
#'
#' @rdname observations_class
#'
#' @keywords internal
setClass(
  Class = "observations",
  slots = c(
    data_predictions = "long_stars",
    parameters = "observation_parameters"
  )
)


#' An S4 class to hold optimization tracing for a starve model
#'
#' @slot opt_time A proc_time object. Time elapsed while computing the ML estimates.
#' @slot hess_time A proc_time object. Time elapsed while computing the hessian matrix.
#' @slot sdr_time A proc_time object. Time elapsed while computing standard errors.
#' @slot parameter_hessian The hessian matrix for parameter estimates.
#' @slot parameter_covariance The covariance matrix for parameter estimates.
#'
#' @rdname tracing_class
#'
#' @family starve_classes
setClass(
  Class = "tracing",
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
#' @rdname TMB_out_class
#'
#' @family starve_classes
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



#' An S4 class describing a starve model.
#'
#' @slot process A process object.
#' @slot observations An observations object.
#' @slot settings A settings object.
#'
#' @rdname starve_class
#'
#' @seealso strv_prepare
#'
#' @family starve_classes
setClass(
  Class = "starve",
  slots = c(
    process = "process",
    observations = "observations",
    tracing = "tracing",
    TMB_out = "TMB_out",
    settings = "settings"
  )
)





######################
### Helper Classes ###
######################

#' An S4 class to collect process and observations parameters.
#'
#' @seealso process_parameters_class, observation_parameters_class, starve_class
#'
#' @rdname parameters_class
#'
#' @family starve_classes
#'
#' @keywords internal
setClass(
  Class = "parameters",
  contains = c(
    "process_parameters",
    "observation_parameters"
  )
)
