#' @include classes.R getset.R generics.R utility.R dag.R staRVe_process_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param time_effects A data.frame
#' @param random_effects An sf object
#' @param persistent_graph A dag object
#' @param parameters A staRVe_process_parameters object
#'
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "staRVe_process",
  definition = function(.Object,
                        time_effects = data.frame(
                          w = numeric(1),
                          se = numeric(1),
                          time = numeric(1)
                        ),
                        random_effects = sf::st_sf(
                          data.frame(
                            w = numeric(1),
                            se = numeric(1),
                            time = numeric(1)
                          ),
                          geometry = sf::st_sfc(sf::st_point())
                        ),
                        persistent_graph = new("dag"),
                        parameters = new("staRVe_process_parameters")) {
    time_effects(.Object)<- time_effects
    random_effects(.Object)<- random_effects
    if( is.null(attr(random_effects(.Object),"active_time")) &&
        "time" %in% colnames(random_effects(.Object)) ) {
      # If active_time attribute doesn't exist but "time" is a column
      # make the active_time attribute to be "time"
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

  # Returns name of covariance function and value of nu
  covariance<- .covariance_from_formula(formula(settings))

  # Return a time column with name and type (ar1/rw/etc) attributes
  time_form<- .time_from_formula(formula(settings),time)
  time_seq<- seq(min(time),max(time))

  # time_effects = "data.frame"
  time_effects(process)<- data.frame(
    w = 0,
    se = NA,
    time = time_seq
  )
  colnames(time_effects(process))[[3]]<- attr(time_form,"name")
  attr(time_effects(process),"time_column")<- attr(time_form,"name")

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

  random_effects(process)<- do.call(rbind,lapply(time_seq,function(t) {
    df<- sf::st_sf(data.frame(w = 0,
                              se = NA,
                              time = t,
                              uniq_nodes))
    colnames(df)[[3]]<- attr(time_form,"name")
    # spatial join of covariates
    covariates<- nodes[nodes[,attr(time_form,"name"),drop=T]==t,]
    covariates<- sf::st_sf(cbind(
      .mean_design_from_space_formula(formula(settings),covariates,"all.vars"),
      covariates[,attr(covariates,"sf_column")]
    ))
    df<- sf::st_join(df,covariates)
    return(df)
  }))
  attr(random_effects(process),"time_column")<- attr(time_form,"name")


  # parameters = "staRVe_process_parameters"
  parameters<- new("staRVe_process_parameters")
  covariance_function(parameters)<- covariance$covariance
  # covariance_function<- takes care of settings spatial parameters,
  # but if nu is supplied for matern need to set nu
  if( covariance$covariance == "matern" & !is.na(covariance$nu) ) {
    spatial_parameters(parameters)["nu","par"]<- covariance$nu
    spatial_parameters(parameters)["nu","fixed"]<- T
  } else {}

  time_parameters(parameters)<- data.frame(
    par = c(0,
            switch(attr(time_form,"type"),
                   ar1 = 0,
                   independent = 0,
                   rw = 1),
            0),
    se = NA,
    fixed = c(F,
              switch(attr(time_form,"type"),
                     ar1 = F,
                     independent = T,
                     rw = T),
              F),
    row.names = c("mu","ar1","sd")
  )
  if( length(unique(time_seq)) == 1 ) {
    # If purely spatial data, we don't need time parameters
    time_parameters(parameters)[c("ar1","sd"),"fixed"]<- c(T,T)
  } else {}

  parameters(process)<- parameters

  return(process)
}
