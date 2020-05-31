#' @include classes.R generics.R staRVe_process_parameters.R staRVe_observation_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_parameters} instead.
#'
#' @export
#' @noRd
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
