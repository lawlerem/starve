#' @include classes.R getset.R generics.R utility.R dag.R long_stars.R staRVe_process_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param time_effects A stars object
#' @param pg_re A stars object, random effects for the persistent graph
#' @param tg_re A stars object, random effects for the transient graph
#' @param persistent_graph A dag object
#' @param transient_graph A dag object
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
                        pg_re = stars::st_as_stars(
                          list(w = array(0,dim=c(1,1,1)),
                               se = array(0,dim=c(1,1,1))),
                          dimensions = stars::st_dimensions(
                            geom = sf::st_sfc(sf::st_point(c(0,0))),
                            time = 0,
                            variable = "y"
                          )
                        ),
                        tg_re = new("long_stars"),
                        persistent_graph = new("dag"),
                        transient_graph = new("dag"),
                        parameters = new("staRVe_process_parameters")) {
    time_effects(.Object)<- time_effects
    pg_re(.Object)<- pg_re
    tg_re(.Object)<- tg_re
    persistent_graph(.Object)<- persistent_graph
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
#' @describeIn staRVe_process Get spatio-temporal random effects for persistent graph
setMethod(f = "pg_re",
          signature = "staRVe_process",
          definition = function(x) return(x@pg_re)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_process Set spatio-temporal random effects for persistent graph
setReplaceMethod(f = "pg_re",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@pg_re<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get spatio-temporal random effects for transient graph
setMethod(f = "tg_re",
          signature = "staRVe_process",
          definition = function(x) return(x@tg_re)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_process Set spatio-temporal random effects for transient graph
setReplaceMethod(f = "tg_re",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@tg_re<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get persistent graph
setMethod(f = "persistent_graph",
          signature = "staRVe_process",
          definition = function(x) return(x@persistent_graph)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn staRVe_process Set persistent graph (for internal use only)
setReplaceMethod(f = "persistent_graph",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@persistent_graph<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get transient graph
setMethod(f = "transient_graph",
          signature = "staRVe_process",
          definition = function(x) return(x@transient_graph)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn staRVe_process Set transient graph (for internal use  only)
setReplaceMethod(f = "transient_graph",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@transient_graph<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get parameters as a staRVe_process_parameters object
setMethod(f = "parameters",
          signature = "staRVe_process",
          definition = function(x) return(x@parameters)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_process Set parameters using a new staRVe_process_parameters
#'   object. Not the recommended way to modify specific parameter values, instead see the
#'   package vignette.
setReplaceMethod(f = "parameters",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@parameters<- value
  return(x)
})




#' @param x An object
#'
#' @export
#' @describeIn staRVe_process Get list containing the persistent graph random
#'   effects and the transient graph random effects.
setMethod(f = "random_effects",
          signature = "staRVe_process",
          definition = function(x) {
  return(list(pg_re = pg_re(x),
              tg_re = tg_re(x)))
})



###############
###         ###
### Utility ###
###         ###
###############

#' Prepare the process part of a staRVe_model object.
#'
#' Creates a new staRVe_process object with the correct dimensions for the
#'   temporal random effects, persistent graph random effects, and transient
#'   graph random effects. Initializes the temporal and spatial parameters for
#'   the model according to the options specified in the formula element of the
#'   settings argument. Constructs the persistent and transient graph.
#'
#'   Before creating the persistent graph any duplicate locations in nodes
#'   are removed. Before creating the transient graph any location in data that
#'   is present in nodes is removed
#'
#'
#' @param data An sf object containing the observation locations and times.
#' @param nodes An sf object containing the persistent graph locations (defaults to data)
#' @param persistent_graph An optionally supplied pre-computed persistent graph.
#'   If a persistent_graph is not supplied, the order of the rows of nodes may change.
#'   If a persisten_graph is supplied, the order will not change. See \link{construct_dag}.
#' @param settings A staRVe_settings object
#'
#' @return A staRVe_process object
#'
#' @seealso staRVe_process
#' @seealso prepare_staRVe_observations, prepare_staRVe_model
#'
#' @keywords internal
prepare_staRVe_process<- function(data,
                                  nodes = data,
                                  persistent_graph = NA,
                                  settings = new("staRVe_settings") ) {
  process<- new("staRVe_process")

  # Return a time column with name and type (ar1/rw/etc) attributes
  time_col<- .time_from_formula(formula(settings),data)
  time_seq<- seq(min(time_col[,1]),max(time_col[,1]))

  # time_effects = "data.frame"
  time_effects(process)<- stars::st_as_stars(
    list(w = array(0,dim=c(length(time_seq),.n_response(formula(settings)))),
         se = array(0,dim=c(length(time_seq),.n_response(formula(settings))))
    ),
    dimensions = stars::st_dimensions(time = time_seq,variable = .response_names(formula(settings)))
  )
  names(stars::st_dimensions(time_effects(process)))[[1]]<- .time_name(settings)


  # pg_re = "sf",
  uniq_nodes<- unique(nodes[,attr(nodes,"sf_column")])

  if( !identical(persistent_graph,NA) ) {
    distance_units(persistent_graph)<- distance_units(settings)
    persistent_graph(process)<- persistent_graph
  } else {
    graph<- construct_dag(uniq_nodes,settings=settings,silent=TRUE)
    uniq_nodes<- graph$locations
    persistent_graph(process)<- graph$dag
  }

  pg_re(process)<- stars::st_as_stars(
    list(w = array(0,dim=c(nrow(uniq_nodes),length(time_seq),.n_response(formula(settings)))),
         se = array(0,dim=c(nrow(uniq_nodes),length(time_seq),.n_response(formula(settings))))
    ),
    dimensions = stars::st_dimensions(geom = sf::st_geometry(uniq_nodes),
                                      time = time_seq,
                                      variable = .response_names(formula(settings)))
  )
  names(stars::st_dimensions(pg_re(process)))[[2]]<- .time_name(settings)



  # tg_re = "sf"
  # Construct transient graph if not supplied
  pg_locs<- .locations_from_stars(pg_re(process))
  colnames(pg_locs)[colnames(pg_locs) == attr(pg_locs,"sf_column")]<- attr(data,"sf_column")
  st_geometry(pg_locs)<- attr(data,"sf_column")
  data<- data[lengths(st_equals(data,pg_locs)) == 0,]

  transient_graph(process)<- construct_transient_dag(
    x = data,
    y = pg_locs,
    time = .time_from_formula(formula(settings),data)[[1]],
    settings = settings
  )
  tg_re(process)<- new("long_stars",
                       locations = sf::st_sf(data.frame(
                          .time_from_formula(formula(settings),data)
                         ),
                         geom = sf::st_geometry(data)
                       ),
                       var_names = .response_names(formula(settings)))
  names(values(tg_re(process)))[[2]]<- "se"
  values(tg_re(process))$linear<- NULL
  values(tg_re(process))$linear_se<- NULL
  values(tg_re(process))$response<- NULL
  values(tg_re(process))$response_se<- NULL



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
      spatial_parameters(parameters)[[i]]["nu","fixed"]<- TRUE
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
      fixed = c(FALSE,
                switch(tt,
                       ar1 = FALSE,
                       independent = TRUE,
                       rw = TRUE),
                FALSE),
      row.names = c("mu","ar1","sd")
    )
    if( length(unique(time_seq)) == 1 ) {
      # If purely spatial data, we don't need time parameters
      df[c("ar1","sd"),"fixed"]<- c(TRUE,TRUE)
    } else {}
    return(df)
  })
  names(time_parameters(parameters))<- .response_names(formula(settings))

  parameters(process)<- parameters

  return(process)
}




#' @param x An sf object
#' @param y A staRVe_process object
#'
#' @describeIn dot-create_graph_idx Locations in the sf object x will be given an
#'   index pointing to either a persistent graph or transient graph location of y.
setMethod(
  f = ".create_graph_idx",
  signature = c("sf","staRVe_process"),
  definition = function(x,y,settings) {
    pg_s<- .locations_from_stars(pg_re(y))
    tg_s<- locations(tg_re(y)) # Should also have a time column
    tg_t<- .time_from_formula(formula(settings),tg_s)[[1]]

    # Get persistent graph locations
    v<- sf::st_equals(x,pg_s)
    v<- do.call(c,lapply(v,function(x) if(length(x)==0) {return(NA)} else {return(x[[1]])}))

    # Get transient graph locations
    splitx<- split(x[is.na(v),],.time_from_formula(formula(settings),x[is.na(v),]))
    splitv<- lapply(splitx,function(t_x) {
      vv<- sf::st_equals(t_x,tg_s[tg_t==staRVe:::.time_from_formula(formula(settings),t_x)[[1]][[1]],])
      vv<- do.call(c,lapply(vv,function(x) if(length(x)==0) {return(NA)} else {return(x[[1]])}))
      vv<- vv+nrow(pg_s)
      return(vv)
    })
    v[is.na(v)]<- do.call(c,splitv)

    if( any(is.na(v)) ) {
      stop("Some data locations do not coincide with any random effect locations. Try re-creating the staRVe_process object.")
    } else {}

    return(v)
})
