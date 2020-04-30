#' @include classes.R dag.R staRVe_observation_parameters.R staRVe_process.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' Create a \code{staRVe_observations} object
#'
#' @export
setGeneric(name = "staRVe_observations",
           def = function(x,...) standardGeneric("staRVe_observations")
)
#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_observations} instead.
#'
#' @export
#' @rdname staRVe_observations
setMethod(
  f = "initialize",
  signature = "staRVe_observations",
  definition = function(.Object,
                        data = sf::st_sf(data.frame(
                            y = numeric(1),
                            time = numeric(1)
                          ),
                          geometry = sf::st_sfc(sf::st_point())
                        ),
                        transient_graph = new("dag"),
                        parameters = new("staRVe_observation_parameters")) {
    data(.Object)<- data
    attr(data,"active_time")<- "time"

    transient_graph(.Object)<- transient_graph
    parameters(.Object)<- parameters

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_observations}.
#'
#' @param x An object of class \code{staRVe_observations}.
#' @param value A replacement value
#'
#' @family Access_staRVe_observations
#' @name Access_staRVe_observations
NULL

#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "data",
           def = function(x) standardGeneric("data")
)
#' @export
setMethod(f = "data",
          signature = "staRVe_observations",
          definition = function(x) return(x@data)
)
#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "data<-",
           def = function(x,value) standardGeneric("data<-")
)
#' @export
setReplaceMethod(f = "data",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@data<- value
  return(x)
})




#' Get or set slots from an object of class \code{staRVe_observations}.
#'
#' @param x An object of class \code{staRVe_observations}.
#' @param value A replacement value
#'
#' @family Access_staRVe_observations
#' @name Access_staRVe_observations
NULL

#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "transient_graph",
           def = function(x) standardGeneric("transient_graph")
)
#' @export
setMethod(f = "transient_graph",
          signature = "staRVe_observations",
          definition = function(x) return(x@transient_graph)
)
#' @export
#' @rdname Access_staRVe_observations
setGeneric(name = "transient_graph<-",
           def = function(x,value) standardGeneric("transient_graph<-")
)
#' @export
setReplaceMethod(f = "transient_graph",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@transient_graph<- value
  return(x)
})




#' Get or set slots from an object of class \code{staRVe_observations}.
#'
#' @param x An object of class \code{staRVe_observations}.
#' @param value A replacement value
#'
#' @family Access_staRVe_observations
#' @name Access_staRVe_observations
NULL

# Defined in staRVe_process.R
# #' @export
# #' @rdname Access_staRVe_observations
# setGeneric(name = "parameters",
#            def = function(x) standardGeneric("parameters")
# )
#' @export
setMethod(f = "parameters",
          signature = "staRVe_observations",
          definition = function(x) return(x@parameters)
)
# Defined in staRVe_process.R
# #' @export
# #' @rdname Access_staRVe_observations
# setGeneric(name = "parameters<-",
#            def = function(x,value) standardGeneric("parameters<-")
# )
#' @export
setReplaceMethod(f = "parameters",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@parameters<- value
  return(x)
})
