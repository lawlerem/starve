#' @include classes.R getset.R generics.R utility.R dag.R staRVe_process_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @noRd
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

#' @export
#' @describeIn staRVe_process Get/set temporal random effects
setMethod(f = "time_effects",
          signature = "staRVe_process",
          definition = function(x) return(x@time_effects)
)
#' @export
setReplaceMethod(f = "time_effects",
                 signature = "staRVe_process",
                 definition = function(x,value) {
  x@time_effects<- value
  return(x)
})

#' @export
#' @describeIn staRVe_process Get/set spatio-temporal random effects
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



#' @export
#' @describeIn staRVe_process Get/set parameters
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

#' Prepare the process part of a staRVe_model object.
#'
#' @param nodes An sf object containing the persistent node locations
#' @param persistent_graph An optionally supplied pre-computed persistent graph
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
  nodes<- nodes[,attr(nodes,"sf_column")] # Only need locations
  nodes<- .order_by_location(unique(nodes)) # Lexicographic ordering S->N/W->E
  random_effects(process)<- do.call(rbind,lapply(time_seq,function(t) {
    df<- sf:::cbind.sf(data.frame(w = 0,
                                  se = NA,
                                  time = t),
                       nodes)
    colnames(df)[[3]]<- attr(time_form,"name")
    return(df)
  }))
  attr(random_effects(process),"time_column")<- attr(time_form,"name")


  # persistent_graph = "dag",
  if( identical(persistent_graph,NA) || class(persistent_graph) != "dag" ) {
    # Create persistent graph
    persistent_graph(process)<- construct_dag(nodes,
      settings = settings,
      silent = T
    )
  } else {
    # Use pre-supplied persistent_graph
    persistent_graph(process)<- persistent_graph
  }

  # parameters = "staRVe_process_parameters"
  parameters<- new("staRVe_process_parameters")
  covariance_function(parameters)<- covariance$covariance
  spatial_parameters(parameters)<- data.frame(
    par = c(0,switch(covariance$covariance,
                     exponential = 0.5,
                     gaussian = Inf,
                     # If nu is supplied, start it at that value,
                     # otherwise start at exponential covariance
                     matern = ifelse(is.nan(covariance$nu),0.5,covariance$nu),
                     matern32 = 1.5)
           ),
    se = NA,
    fixed = c(F,switch(covariance$covariance,
                       exponential = T,
                       gaussian = T,
                       # If nu is supplied, fix it at that value
                       matern = ifelse(is.nan(covariance$nu),F,T),
                       matern32 = T)
             ),
    row.names = c("sd","nu")
  )


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
    time_parameters(parameters)$fixed[c("ar1","sd")]<- c(T,T)
  } else {}

  parameters(process)<- parameters

  return(process)
}
