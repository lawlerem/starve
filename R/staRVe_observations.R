#' @include classes.R generics.R dag.R staRVe_observation_parameters.R staRVe_process.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @noRd
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
    dat(.Object)<- data
    if( is.null(attr(dat(.Object),"active_time")) &&
        "time" %in% colnames(dat(.Object)) ) {
      # If active_time attribute doesn't exist but "time" is a column
      # make the active_time attribute to be "time"
      attr(dat(.Object),"active_time")<- "time"
    } else {}

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


#' @export
#' @describeIn staRVe_observations Get/set data
setMethod(f = "dat",
          signature = "staRVe_observations",
          definition = function(x) return(x@data)
)
#' @export
setReplaceMethod(f = "dat",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@data<- value
  return(x)
})



#' Get/set transient graph
#'
#' @noRd
setMethod(f = "transient_graph",
          signature = "staRVe_observations",
          definition = function(x) return(x@transient_graph)
)
setReplaceMethod(f = "transient_graph",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@transient_graph<- value
  return(x)
})



#' @export
#' @describeIn staRVe_observations Get/set parameters
setMethod(f = "parameters",
          signature = "staRVe_observations",
          definition = function(x) return(x@parameters)
)
#' @export
setReplaceMethod(f = "parameters",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@parameters<- value
  return(x)
})



###############
###         ###
### Utility ###
###         ###
###############

#' Prepare the observation part of a staRVe_model object.
#'
#' @param data An sf object containing the observations and covariates.
#' @param process A staRVe_process object.
#' @param transient_graph An optionally supplied pre-computed transient graph
#' @param settings A staRVe_settings object.
#' @param distribution The response distribution to use. must be one given by
#'   get_staRVe_distributions("distribution").
#' @param link The link function to use. must be one given by
#'   get_staRVe_distributions("link").
#'
#' @return A staRVe_observations object.
#'
#' @noRd
prepare_staRVe_observations<- function(data,
                                       process,
                                       transient_graph = NA,
                                       settings = new("staRVe_settings"),
                                       distribution = "gaussian",
                                       link = "default") {
  observations<- new("staRVe_observations")

  # Return a time column with name and type (ar1/rw/etc) attributes
  # Need it here for the name attribute
  time_form<- .time_from_formula(formula(settings),data)

  # data = "sf"
  # Put in lexicographic ordering by time, then S->N / W->E
  data<- .order_by_location(data,time = data[[attr(time_form,"name")]])
  # Returns response variable with a "name" attribute
  y<- .response_from_formula(formula(settings),data)
  # Return a time column with name and type (ar1/rw/etc) attributes
  time_form<- .time_from_formula(formula(settings),data) #in order
  response<- data.frame(y = c(y),t = c(time_form)) # c() removes attributes
  names(response)<- c(attr(y,"name"),attr(time_form,"name"))

  # Get covariates, and sample size information if using a binomial response
  design<- .mean_design_from_formula(formula(settings),data,return = "model.frame")
  sample_size<- .sample_size_from_formula(formula(settings),data)

  dat(observations)<- sf:::cbind.sf(
    w = 0,
    w_se = NA,
    linear = NA,
    linear_se = NA,
    response = NA,
    response_se = NA,
    design,
    sample_size,
    response,
    data[,attr(data,"sf_column")]
  )
  attr(dat(observations),"time_column")<- attr(time_form,"name")



  # transient_graph = "dag"
  # Random effect locations are the same each year, so only need first year
  random_effects<- split(
    random_effects(process),
    random_effects(process)[,attr(random_effects(process),"time_column"),drop=T]
  )[[1]]
  if( identical(transient_graph,NA) || class(transient_graph) != "dag" ) {
    # Construct transient graph if not supplied
    transient_graph(observations)<- construct_obs_dag(
      x = data,
      y = random_effects,
      settings = new("staRVe_settings"),
    )
  } else {
    # Use pre-supplied transient graph
    transient_graph(observations)<- transient_graph
  }


  # parameters = "staRVe_observation_parameters"
  parameters<- new("staRVe_observation_parameters")

  # Match supplied response distribution to valid options
  # response_distribution<- also takes care of response parameters
  # and link function
  response_distribution(parameters)<- unname(
    get_staRVe_distributions("distribution")[
      charmatch(distribution,get_staRVe_distributions("distribution"))
    ]
  )

  # Match supplied link function with valid options
  if( !identical(link,"default") ) {
    link_function(parameters)<- unname(
      get_staRVe_distributions("link")[
        charmatch(link,get_staRVe_distributions("link"))
      ]
    )
  } else {}

  # Set up fixed effects according to covariates formula
  design<- .mean_design_from_formula(formula(settings),data)
  fixed_effects(parameters)<- data.frame(
    par = numeric(ncol(design)),
    se = rep(NA,ncol(design)),
    fixed = rep(F,ncol(design)),
    row.names = colnames(design)
  )
  parameters(observations)<- parameters

  return(observations)
}
