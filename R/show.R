#' @include classes.R generics.R
NULL

#' Print method for objects of class \code{dag}.
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "dag",
          definition = function(object) {
  n_nodes<- length(edges(object))
  avg_deg<- median(do.call(c,lapply(edges(object),length)))
  avg_dist<- mean(do.call(c,lapply(distances(object),c)))
  cat("\n")
  print(paste0("A directed acyclic graph with ",n_nodes,
               " nodes, with an median in-degree of ",avg_deg,"."))
  print(paste0("The average edge distance is ",round(avg_dist,2),"",
               distance_units(object),"."))

  return(invisible())
})

#' Print method for object of class \code{staRVe_process_parameters}
#'
#' @export
#' @noRd
NULL
# setMethod(f = "show",
#           signature = "staRVe_process_parameters",
#           definition = function(object) {
# Default is fine
# })

#' Print method for object of class \code{staRVe_process}
#'
#' @export
#' @noRd
NULL
# setMethod(f = "show",
#           signature = "staRVe_process",
#           definition = function(object) {
# Default is fine
# })

#' Print method for object of class \code{staRVe_observation_parameters}
#'
#' @export
#' @noRd
NULL
# setMethod(f = "show",
#           signature = "staRVe_observation_parameters",
#           definition = function(object) {
# Default is fine
# })

#' Print method for object of class \code{staRVe_observations}
#'
#' @export
#' @noRd
NULL
# setMethod(f = "show",
#           signature = "staRVe_observations",
#           definition = function(object) {
# Default is fine
# })

#' Print method for object of class \code{staRVe_settings}
#'
#' @export
#' @noRd
NULL
# setMethod(f = "show",
#           signature = "staRVe_settings",
#           definition = function(object) {
# Default is fine
# })

#' Print method for object of class \code{staRVe_model}
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "staRVe_model",
          definition = function(object) {
  cat("\n")
  print(parameters(object))
  cat("\n")
  # cat("Random Effects")
  # cat("\n")
  # print(random_effects(object))
  # cat("\n")
  cat("Data")
  cat("\n")
  print(dat(object))

  return(invisible())
})

#' Print method for object of class \code{staRVe_tracing}
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "staRVe_tracing",
          definition = function(object) {
  cat("\n")
  cat("Tracing information for a staRVe_model fit.")
  cat("\n")
  print(timing(object))

  return(invisible())
})

#' Print method for object of class \code{TMB_out}
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "TMB_out",
          definition = function(object) {
  cat("\n")
  cat("An class containing TMB objects: obj, opt, and sdr.")
  cat("\n")

  return(invisible())
})

#' Print method for object of class \code{staRVe_fit}
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "staRVe_fit",
          definition = function(object) {
  cat("\n")
  print(convergence(object))
  print(as(object,"staRVe_model"))

  return(invisible())
})

#' Print method for object of class \code{staRVe_parameters}
#'
#' @export
#' @noRd
NULL
# setMethod(f = "show",
#           signature = "staRVe_parameters",
#           definition = function(object) {
# Default is fine
# })
