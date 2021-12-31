#' @include classes.R getset.R generics.R utility.R dag.R staRVe_process_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param time_effects A stars object
#' @param random_effects A stars object
#' @param persistent_graph A dag object
#' @param parameters A staRVe_process_parameters object
#'
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "staRVe_process",
  definition = function(.Object,
                        time_effects = stars::st_as_stars(
                          list(w = array(0,dim=c(1,1)),
                               se = array(0,dim=c(1,1))),
                          dimensions = stars::st_dimensions(time=0,variable="y")
                        ),
                        random_effects = stars::st_as_stars(
                          list(w = array(0,dim=c(1,1,1)),
                               se = array(0,dim=c(1,1,1))),
                          dimensions = stars::st_dimensions(
                            geom = sf::st_sfc(sf::st_point(c(0,0))),
                            time = 0,
                            variable = "y"
                          )
                        ),
                        persistent_graph = new("dag"),
                        parameters = new("staRVe_process_parameters")) {
    time_effects(.Object)<- time_effects
    random_effects(.Object)<- random_effects
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

#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get temporal random effects
setMethod(f = "time_effects",
          signature = "staRVe_process",
          definition = function(x) return(x@time_effects)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_process Set temporal random effects
setReplaceMethod(f = "time_effects",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@time_effects<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get spatio-temporal random effects
setMethod(f = "random_effects",
          signature = "staRVe_process",
          definition = function(x) return(x@random_effects)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_process Set spatio-temporal random effects
setReplaceMethod(f = "random_effects",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@random_effects<- value
  return(x)
})



#' Get/set persistent graph
#'
#' @noRd
setMethod(f = "persistent_graph",
          signature = "staRVe_process",
          definition = function(x) return(x@persistent_graph)
)
setReplaceMethod(f = "persistent_graph",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@persistent_graph<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Set parameters
setMethod(f = "parameters",
          signature = "staRVe_process",
          definition = function(x) return(x@parameters)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_process Set parameters
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

#' Prepare the process part of a staRVe_model object.
#'
#' @param nodes An sf object containing the persistent node locations, and covariate
#'   values if included in space(...) special of formula.
#' @param persistent_graph An optionally supplied pre-computed persistent graph.
#'   If a persistent_graph is not supplied, the order of the rows of nodes may change.
#'   If a persisten_graph is supplied, the order will not change.
#' @param time A data.frame containing at least one column, which contains the
#'   time index
#' @param settings A staRVe_settings object
#'
#' @return A staRVe_process object
#'
#' @noRd
prepare_staRVe_process<- function(nodes,
                                  persistent_graph = NA,
                                  time = data.frame(time=0),
                                  settings = new("staRVe_settings") ) {
  process<- new("staRVe_process")

  # Return a time column with name and type (ar1/rw/etc) attributes
  time_col<- .time_from_formula(formula(settings),time)
  time_seq<- seq(min(time_col[,1]),max(time_col[,1]))

  # time_effects = "data.frame"
  time_effects(process)<- stars::st_as_stars(
    list(w = array(0,dim=c(length(time_seq),.n_response(formula(settings)))),
         se = array(0,dim=c(length(time_seq),.n_response(formula(settings))))
    ),
    dimensions = stars::st_dimensions(time = time_seq,variable = .response_names(formula(settings)))
  )
  names(stars::st_dimensions(time_effects(process)))[[1]]<- .time_name(settings)


  # random_effects = "sf",
  uniq_nodes<- unique(nodes[,attr(nodes,"sf_column")])

  if( !identical(persistent_graph,NA) ) {
    distance_units(persistent_graph)<- distance_units(settings)
    persistent_graph(process)<- persistent_graph
  } else {
    graph<- construct_dag(uniq_nodes,settings=settings,silent=T)
    uniq_nodes<- graph$locations
    persistent_graph(process)<- graph$dag
  }

  random_effects(process)<- stars::st_as_stars(
    list(w = array(0,dim=c(nrow(uniq_nodes),length(time_seq),.n_response(formula(settings)))),
         se = array(0,dim=c(nrow(uniq_nodes),length(time_seq),.n_response(formula(settings))))
    ),
    dimensions = stars::st_dimensions(geom = sf::st_geometry(uniq_nodes),
                                      time = time_seq,
                                      variable = .response_names(formula(settings)))
  )
  names(stars::st_dimensions(random_effects(process)))[[2]]<- .time_name(settings)


  # parameters = "staRVe_process_parameters"
  parameters<- new("staRVe_process_parameters")
  # Returns name of covariance function and value of nu
  covariance<- .covariance_from_formula(formula(settings))

  covariance_function(parameters)<- covariance$covariance
  # covariance_function<- takes care of settings spatial parameters,
  # but if nu is supplied for matern need to set nu
  for( i in seq_along(covariance$nu) ) {
    if( covariance$covariance[[i]] == "matern" & !is.na(covariance$nu[[i]]) ) {
      spatial_parameters(parameters)[[i]]["nu","par"]<- covariance$nu[[i]]
      spatial_parameters(parameters)[[i]]["nu","fixed"]<- T
    } else {}
  }
  names(spatial_parameters(parameters))<- .response_names(formula(settings))

  time_parameters(parameters)<- lapply(attr(time_col,"type"),function(tt) {
    df<- data.frame(
      par = c(0,
              switch(tt,
                     ar1 = 0,
                     independent = 0,
                     rw = 1),
              0),
      se = NA,
      fixed = c(F,
                switch(tt,
                       ar1 = F,
                       independent = T,
                       rw = T),
                F),
      row.names = c("mu","ar1","sd")
    )
    if( length(unique(time_seq)) == 1 ) {
      # If purely spatial data, we don't need time parameters
      df[c("ar1","sd"),"fixed"]<- c(T,T)
    } else {}
    return(df)
  })
  names(time_parameters(parameters))<- .response_names(formula(settings))

  parameters(process)<- parameters

  return(process)
}
