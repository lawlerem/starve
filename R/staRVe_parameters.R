#' @include classes.R generics.R staRVe_process_parameters.R staRVe_observation_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param process_parameters A staRVe_process_parameters object
#' @param observation_parameters A staRVe_observation_parameters object
#'
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "staRVe_parameters",
  definition = function(.Object,
                        process_parameters = new("staRVe_process_parameters"),
                        observation_parameters = new("staRVe_observation_parameters")) {
    as(.Object,"staRVe_process_parameters")<- process_parameters
    as(.Object,"staRVe_observation_parameters")<- observation_parameters

    return(.Object)
  }
)
