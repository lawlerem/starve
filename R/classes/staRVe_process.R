#' @include classes.R generics.R dag.R staRVe_process_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_process} instead.
#'
#' @export
#' @rdname staRVe_process
setMethod(
  f = "initialize",
  signature = "staRVe_process",
  definition = function(.Object,
                        random_effects = sf::st_sf(data.frame(
                            w = numeric(1),
                            fixed = logical(1),
                            time = numeric(1)
                          ),
                          geometry = sf::st_sfc(sf::st_point())
                        ),
                        persistent_graph = new("dag"),
                        parameters = new("staRVe_process_parameters")) {
    data(.Object)<- data
    if( !is.null(attr(data,"active_time")) && "time" %in% colnames(data) ) {
      attr(data,"active_time")<- "time"
    else {}

    persistent_graph(.Object)<- persistent_graph
    parameters(.Object)<- parameters

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_process}.
#'
#' @param x An object of class \code{staRVe_process}.
#' @param value A replacement value
#'
#' @family Access_staRVe_process
#' @name Access_staRVe_process
NULL

#' @export
setMethod(f = "random_effects",
          signature = "staRVe_process",
          definition = function(x) return(x@random_effects)
)
#' @export
setReplaceMethod(f = "random_effects",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@random_effects<- value
  return(x)
})



#' @export
setMethod(f = "persistent_graph",
          signature = "staRVe_process",
          definition = function(x) return(x@persistent_graph)
)
#' @export
setReplaceMethod(f = "persistent_graph",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@persistent_graph<- value
  return(x)
})



#' @export
setMethod(f = "parameters",
          signature = "staRVe_process",
          definition = function(x) return(x@parameters)
)
#' @export
setReplaceMethod(f = "parameters",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@parameters<- value
  return(x)
})
