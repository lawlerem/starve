#' An S4 class to hold a directed acyclic graph with distances.
setClass(
  Class = "dag",
  slots = c(
    edges = "list",
    distances = "list"
  )
)

#' An S4 class to hold process parameters for a staRVe model.
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
#' @family staRVe-classes
setClass(
  Class = "staRVe_process",
  slots = c(
    random_effects = "sf",
    persistent_graph = "dag",
    parameters = "staRVe_process_parameters"
  ),
)

#' An S4 class to hold observation parameters for a staRVe model.
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
#' @family staRVe-classes
setClass(
  Class = "staRVe_observations",
  slots = c(
    data = "sf",
    transient_graph = "dag",
    parameters = "staRVe_observation_parameters"
  )
)

#' An S4 class to hold extra settings for a staRVe model
setClass(
  Class = "staRVe_settings",
  slots = c(
    formula = "formula",
    n_neighbours = "numeric",
    p_far_neighbours = "numeric",
    distance_units = "character",
    max_distance = "numeric"
  )
)

#' An S4 class to hold the input for a staRVe model
setClass(
  Class = "staRVe_model",
  slots = c(
    process = "staRVe_process",
    observations = "staRVe_observations",
    settings = "staRVe_settings"
  )
)

#' An S4 class to hold optimization tracing for a staRVe model
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

#' An S4 class to hold a fitted staRVe model
setClass(
  Class = "staRVe_fit",
  slots = c(
    tracing = "staRVe_tracing",
    TMB_out = "TMB_out"
  ),
  contains = c("staRVe_model")
)
