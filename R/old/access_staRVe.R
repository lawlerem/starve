#' @include classes.R access_TMB_out.R
NULL

###########################
###                     ###
###  Construct_staRVe   ###
###                     ###
###########################

#' Create a \code{staRVe} object.
#' @param x An object to coerce to class \code{staRVe}.
#'
#' @export
setGeneric(name = "staRVe",
           def = function(x,...) standardGeneric("staRVe")
)
#' @details The \code{initialize} function is not meant to be used by the user,
#'  use \code{staRVe} instead.
#'
#' @export
#' @rdname staRVe
setMethod(
    f = "initialize",
    signature = "staRVe",
    definition = function(.Object,
                          TMB_out = new("TMB_out"),
                          observation = sf::st_sf(data.frame(
                                                y = numeric(1),
                                                response = numeric(1),
                                                response_se = numeric(1),
                                                residual = numeric(1)
                                            ),
                                            geometry = sf::st_sfc(sf::st_point())
                          ),
                          process = sf::st_sf(data.frame(
                                                w = numeric(1),
                                                w_se = numeric(1)
                                            ),
                                            geometry = sf::st_sfc(sf::st_point())
                          ),
                          parameters = data.frame(parameter = character(0),
                                                  estimate = numeric(0),
                                                  se = numeric(0)),
                          convergence = character(0),
                          settings = list(n_neighbours = numeric(0),
                                         distance_units = structure(numeric(0),class="units"))) {
        as(.Object,"TMB_out")<- TMB_out
        observation(.Object)<- observation
        process(.Object)<- process
        parameters(.Object)<- parameters
        convergence(.Object)<- convergence
        settings(.Object)<- settings

        return(.Object)
    }
)
#' @export
setMethod(f = "staRVe",
          signature = "TMB_out",
          def = function(x,Observation_sf,Process_sf,settings) {
        return(new("staRVe",
                   TMB_out = x,
                   observation = get_observation(x,Observation_sf),
                   process = get_process(x,Process_sf),
                   parameters = get_parameters(x),
                   convergence = convergence(x),
                   settings = settings
               )
           )
    }
)




#######################
###                 ###
###  Access_staRVe  ###
###                 ###
#######################

#' Get or set slots from an object of class \code{staRVe}.
#'
#' @param x An object of class \code{staRVe}.
#' @param value A replacement value.
#'
#' @family Access_staRVe
#' @name Access_staRVe
NULL

#' @export
#' @rdname Access_staRVe
setGeneric(name = "observation",
           def = function(x) standardGeneric("observation")
)
#' @export
setMethod(f = "observation",
          signature = "staRVe",
          definition = function(x) return(x@observation)
)
#' @export
#' @rdname Access_staRVe
setGeneric(name = "observation<-",
           def = function(x,value) standardGeneric("observation<-")
)
#' @export
setReplaceMethod(f = "observation",
                 signature = "staRVe",
                 definition = function(x,value) {
                     x@observation<- value
                     return(x)
                 }
)


#' @export
#' @rdname Access_staRVe
setGeneric(name = "process",
           def = function(x) standardGeneric("process")
)
#' @export
setMethod(f = "process",
          signature = "staRVe",
          definition = function(x) return(x@process)
)
#' @export
#' @rdname Access_staRVe
setGeneric(name = "process<-",
           def = function(x,value) standardGeneric("process<-")
)
#' @export
setReplaceMethod(f = "process",
                 signature = "staRVe",
                 definition = function(x,value) {
                     x@process<- value
                     return(x)
                 }
)


#' @export
#' @rdname Access_staRVe
setGeneric(name = "parameters",
           def = function(x) standardGeneric("parameters")
)
#' @export
setMethod(f = "parameters",
          signature = "staRVe",
          definition = function(x) return(x@parameters)
)
#' @export
#' @rdname Access_staRVe
setGeneric(name = "parameters<-",
           def = function(x,value) standardGeneric("parameters<-")
)
#' @export
setReplaceMethod(f = "parameters",
                 signature = "staRVe",
                 definition = function(x,value) {
                     x@parameters<- value
                     return(x)
                 }
)


#' @export
#' @rdname Access_staRVe
setGeneric(name = "convergence",
           def = function(x) standardGeneric("convergence")
)
#' @export
setMethod(f = "convergence",
          signature = "staRVe",
          definition = function(x) return(x@convergence)
)
#' @export
#' @rdname Access_staRVe
setGeneric(name = "convergence<-",
           def = function(x,value) standardGeneric("convergence<-")
)
#' @export
setReplaceMethod(f = "convergence",
                 signature = "staRVe",
                 definition = function(x,value) {
                     x@convergence<- value
                     return(x)
                 }
)


#' @export
#' @rdname Access_staRVe
setGeneric(name = "settings",
           def = function(x) standardGeneric("settings")
)
#' @export
setMethod(f = "settings",
          signature = "staRVe",
          definition = function(x) return(x@settings)
)
#' @export
#' @rdname Access_staRVe
setGeneric(name = "settings<-",
           def = function(x,value) standardGeneric("settings<-")
)
#' @export
setReplaceMethod(f = "settings",
                 signature = "staRVe",
                 definition = function(x,value) {
                     x@settings<- value
                     return(x)
                 }
)
