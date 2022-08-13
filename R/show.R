#' @include classes.R getset.R generics.R
NULL

#' @param object An object
#'
#' @describeIn dag_class Print method
setMethod(
    f = "show",
    signature = "dag",
    definition = function(object) {
  n_nodes<- length(edges(object))
  cat("\n")
  if( n_nodes > 0 ) {
    avg_deg<- median(do.call(c,lapply(edges(object),function(x) length(x[[2]]))))
    avg_dist<- mean(do.call(c,lapply(distances(object),c)))
    print(paste0("A directed acyclic graph with ",n_nodes,
                 " nodes, with an median in-degree of ",avg_deg,"."))
    print(paste0("The average edge distance is ",round(avg_dist,2),"",
                 distance_units(object),"."))
  } else {
    print(paste0("An empty directed acyclic graph."))
  }

  return(invisible(object))
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
#' @describeIn tracing_class Print method
setMethod(
    f = "show",
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

  return(invisible(object))
})

#' @param object An object
#'
#' @export
#' @describeIn TMB_out_class Print method
setMethod(
    f = "show",
    signature = "TMB_out",
    definition = function(object) {
  cat("\n")
  cat("An class containing TMB objects: obj, opt, and sdr.")
  cat("\n")

  return(invisible(object))
})



#' @param object An object
#'
#' @export
#' @describeIn starve_class Print method
setMethod(
    f = "show",
    signature = "starve",
    definition = function(object) {
  cat("A starve model object\n\n")
  cat("Model formula: ")
  print(formula(object))

  cat("Response distribution: ")
  cat(response_distribution(object))
  cat("\n")

  cat("Link function: ")
  cat(link_function(object))
  cat("\n")

  cat("Optimizer message: ")
  cat(convergence(object))
  cat("\n\n")


  cat(paste("Data is a simple feature collection with",nrow(dat(object)),"features\n"))
  cat("CRS: ")
  cat(format(sf::st_crs(dat(object))))
  cat("\n")
  cat("Bounding times: ")
  t<- time_from_formula(formula(object),dat(object))
  cat(c(min(t),max(t)))
  cat("\n")
  cat("Bounding box:\n")
  print(sf::st_bbox(dat(object)))
  cat("\n")


  return(invisible(object))
})


# setMethod(f = "show",
#           signature = "parameters",
#           definition = function(object) {
# Default is fine
# })
