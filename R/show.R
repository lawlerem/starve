#' @include classes.R getset.R generics.R
NULL

#' @param object An object
#'
#' @export
#' @describeIn dag_class Print method
setMethod(f = "show",
          signature = "dag",
          definition = function(object) {
  n_nodes<- length(edges(object))
  avg_deg<- median(do.call(c,lapply(edges(object),function(x) length(x[[2]]))))
  avg_dist<- mean(do.call(c,lapply(distances(object),c)))
  cat("\n")
  print(paste0("A directed acyclic graph with ",n_nodes,
               " nodes, with an median in-degree of ",avg_deg,"."))
  print(paste0("The average edge distance is ",round(avg_dist,2),"",
               distance_units(object),"."))

  return(invisible())
})

# setMethod(f = "show",
#           signature = "process_parameters",
#           definition = function(object) {
# Default is fine
# })

# setMethod(f = "show",
#           signature = "process",
#           definition = function(object) {
# Default is fine
# })

# setMethod(f = "show",
#           signature = "observation_parameters",
#           definition = function(object) {
# Default is fine
# })

# setMethod(f = "show",
#           signature = "observations",
#           definition = function(object) {
# Default is fine
# })

# setMethod(f = "show",
#           signature = "settings",
#           definition = function(object) {
# Default is fine
# })

#' @param object An object
#'
#' @export
#' @describeIn starve_class Print method
setMethod(f = "show",
          signature = "starve",
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

#' @param object An object
#'
#' @export
#' @describeIn tracing_class Print method
setMethod(f = "show",
          signature = "tracing",
          definition = function(object) {
  cat("\n")
  cat("Time elapsed while fitting the model:")
  cat("\n")
  print(timing(object))
  cat("\n\n")
  cat("Estimated parameter hessian matrix:")
  cat("\n")
  print(parameter_hessian(object))
  cat("\n\n")
  cat("Estimated parameter covariance matrix:")
  cat("\n")
  print(parameter_covariance(object))
  cat("\n")

  return(invisible())
})

#' @param object An object
#'
#' @export
#' @describeIn TMB_out_class Print method
setMethod(f = "show",
          signature = "TMB_out",
          definition = function(object) {
  cat("\n")
  cat("An class containing TMB objects: obj, opt, and sdr.")
  cat("\n")

  return(invisible())
})

#' @param object An object
#'
#' @export
#' @describeIn starve_fit_class Print method
setMethod(f = "show",
          signature = "starve_fit",
          definition = function(object) {
  cat("\n")
  print(convergence(object))
  print(as(object,"starve"))

  return(invisible())
})

# setMethod(f = "show",
#           signature = "parameters",
#           definition = function(object) {
# Default is fine
# })
