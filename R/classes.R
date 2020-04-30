#' An S4 class to hold the raw TMB output of \code{fit_catch_data}.
#'
#' @family staRVe-classes
#'
#' @slot obj A list. The output of \code{TMB::MakeADFun}.
#' @slot opt A list. The output of \code{nlminb}.
#' @slot sdr An object of class \code{sdreport}. The output of \code{TMB::sdreport(obj)}.
#' @slot symbolicAnalysis A logical value. Was runOrderings used to fit the model?
#'
#' @export
setClass(
    Class = "TMB_out",
    slots = c(
        obj = "list",
        opt = "list",
        sdr = "sdreport",
        symbolicAnalysis = "logical",
        TMB_in = "list"
    )
)


#' An S4 class to hold results from an analysis of RV data.
#'
#' @family staRVe-classes
#'
#' @slot TMB_out An object of class \code{TMB_out} holding the raw TMB output of the model.
#' @slot Observation An \code{sf} object containing data.frame with columns \code{y}, \code{response}, \code{response_se}, and \code{residual}, and an \code{sfc} object containing location data.
#' @slot Process An \code{sf} object containing data.frame with columns \code{w} and \code{w_se}, and an \code{sfc} object containing location data.
#' @slot parameters A data.frame containing the columns \code{par}, and \code{par_se}.
#' @slot convergence A character vector giving the convergence message.
#' @slot settings A list with elements: n_neighbours, distance_units, formula,
#'   distribution_code, link_code, and time_column.
#'
#' @export
setClass(
    Class = "staRVe",
    slots = c(
        observation = "sf",
        process = "sf",
        parameters = "data.frame",
        convergence = "character",
        settings = "list"
    ),
    contains = "TMB_out"
)






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
    distance_units = "character"
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
