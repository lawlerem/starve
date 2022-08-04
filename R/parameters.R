#' @include classes.R generics.R process_parameters.R observation_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param process_parameters A process_parameters object
#' @param observation_parameters A observation_parameters object
#'
#' @noRd
setMethod(
  f = "initialize",
  signature = "parameters",
  definition = function(.Object,
                        process_parameters = new("process_parameters"),
                        observation_parameters = new("observation_parameters")) {
    as(.Object,"process_parameters")<- process_parameters
    as(.Object,"observation_parameters")<- observation_parameters

    return(.Object)
  }
)
