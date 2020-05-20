#' @include classes.R generics.R utility.R dag.R staRVe_process_parameters.R
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
    random_effects(.Object)<- random_effects
    if( !is.null(attr(random_effects(.Object),"active_time")) &&
        "time" %in% colnames(random_effects(.Object)) ) {
      attr(random_effects(.Object),"active_time")<- "time"
    } else {}

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



###############
###         ###
### Utility ###
###         ###
###############

prepare_staRVe_process<- function(nodes,
                                  time = data.frame(time=0),
                                  settings = new("staRVe_settings") ) {
  process<- new("staRVe_process")

  covariance<- .covariance_from_formula(formula(settings))
  time_form<- .time_from_formula(formula(settings),time)

  # random_effects = "sf",
  nodes<- nodes[,attr(nodes,"sf_column")] # Only need locations
  nodes<- order_by_location(unique(nodes))
  time_seq<- seq(min(time),max(time))
  random_effects(process)<- do.call(rbind,lapply(time_seq,function(t) {
    df<- sf:::cbind.sf(data.frame(w = 0,
                                  se = NA,
                                  fixed = F,
                                  time = t),
                       nodes)
    colnames(df)[[4]]<- attr(time_form,"name")
    return(df)
  }))
  attr(random_effects(process),"time_column")<- attr(time_form,"name")


  # persistent_graph = "dag",
  persistent_graph(process)<- construct_dag(nodes,
    settings = settings,
    silent = T
  )

  # parameters = "staRVe_process_parameters"
  parameters<- new("staRVe_process_parameters")
  covariance_function(parameters)<- covariance$covariance
  spatial_parameters(parameters)<- data.frame(
    par = c(0,0,ifelse(is.nan(covariance$nu),0,covariance$nu)),
    se = NA,
    fixed = c(F,F,ifelse(is.nan(covariance$nu),F,T)),
    row.names = c("rho","tau","nu")
  )


  time_parameters(parameters)<- data.frame(
    par = c(switch(attr(time_form,"type"),
                   ar1 = 0.5,
                   independent = 0,
                   rw = 1)),
    se = NA,
    fixed = c(switch(attr(time_form,"type"),
                   ar1 = F,
                   independent = T,
                   rw = T)),
    row.names = c("phi")
  )

  parameters(process)<- parameters

  return(process)
}
