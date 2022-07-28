#' @include classes.R generics.R dag.R staRVe_observation_parameters.R staRVe_process.R long_stars.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param data_predictions A long_stars object
#' @param transient_graph A dag object
#' @param parameters An observation_parameters object
#'
#' @rdname starve-construct
setMethod(
  f = "initialize",
  signature = "observations",
  definition = function(.Object,
                        data_predictions = new("long_stars"),
                        parameters = new("observation_parameters")) {
    data_predictions(.Object)<- data_predictions
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
#' @describeIn observations_class Get data, including response variables,
#'   time indices, locations, covariates, etc.
setMethod(f = "dat",
          signature = "observations",
          definition = function(x) return(locations(x@data_predictions))
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn observations_class Set data. Warning: if you want to add new
#'   rows you must give each additional row a value in the graph_idx column,
#'   or create a new object from scratch.
setReplaceMethod(f = "dat",
                 signature = "observations",
                 definition = function(x,value) {
  locations(x@data_predictions)<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn observations_class Get data_predictions, a long_stars object
#'   with the data (see \code{dat}) and associated random effect predictions.
setMethod(f = "data_predictions",
          signature = "observations",
          definition = function(x) return(x@data_predictions)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn observations_class Set data predictions
setReplaceMethod(f = "data_predictions",
                 signature = "observations",
                 definition = function(x,value) {
  x@data_predictions<- value
  return(x)
})




#' @param x An object
#'
#' @export
#' @describeIn observations_class Get parameters as an
#'   observation_parameters object.
setMethod(f = "parameters",
          signature = "observations",
          definition = function(x) return(x@parameters)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn observations_class Set parameters using a new
#'   observation_parameters object. Not the recommended way to
#'   modify specific parmaeter values, instead see the package vignette
setReplaceMethod(f = "parameters",
                 signature = "observations",
                 definition = function(x,value) {
  x@parameters<- value
  return(x)
})



###############
###         ###
### Utility ###
###         ###
###############

#' Prepare the observation part of a starve object.
#'
#' Creates a new observation object with the correct dimensions
#'   for the random effect predictions. Initializes the response distribution
#'   and fixed effect parameters for the model according to the options specified
#'   in the formula element of the settings argument. Also adds a column "graph_idx"
#'   to the supplied data.
#'
#' @param data An sf object containing the observations and covariates.
#' @param process A process object.
#' @param settings A settings object.
#' @param distribution The response distribution to use. Must be one given by
#'   get_starve_distributions("distribution").
#' @param link The link function to use. must be one given by
#'   get_starve_distributions("link").
#'
#' @return An observations object.
#'
#' @seealso observations_class
#' @seealso strv_prepare_process, strv_prepare
#'
#' @keywords internal
strv_prepare_observations<- function(data,
                                     process,
                                     settings = new("settings"),
                                     distribution = "gaussian",
                                     link = "default") {
  observations<- new("observations")

  # data = "sf"
  # Put in lexicographic ordering by time, then S->N / W->E
  data<- order_by_location(data,time = data[[time_name(settings)]])

  data_predictions(observations)<- new("long_stars",
    sf::st_sf(data.frame(
      mean_design_from_formula(formula(settings),data,return = "all.vars"),
      sample_size_from_formula(formula(settings),data,unique_vars = TRUE),
      response_from_formula(formula(settings),data),
      time_from_formula(formula(settings),data,"column"),
      graph_idx = create_graph_idx(data,process,settings),
      data[,attr(data,"sf_column")]
    )),
    var_names = response_names(formula(settings))
  )



  # parameters = "observation_parameters"
  parameters<- new("observation_parameters")

  # Match supplied response distribution to valid options
  # response_distribution<- also takes care of response parameters
  # and link function
  response_distribution(parameters)<- rep(distribution,length.out=n_response(formula(settings)))
  names(response_parameters(parameters))<- response_names(formula(settings))

  # Match supplied link function with valid options
  if( !identical(link,"default") ) {
    link_function(parameters)<- rep(link,length.out=n_response(formula(settings)))
  } else {}

  # Set up fixed effects according to covariates formula
  nff<- colnames(mean_design_from_formula(formula(settings),data))
  fixed_effects(parameters)<- lapply(response_names(formula(settings)),function(rr) {
    return(data.frame(
      par = numeric(length(nff)),
      se = rep(NA,length(nff)),
      fixed = rep(FALSE,length(nff)),
      row.names = nff
    ))
  })
  names(fixed_effects(parameters))<- response_names(formula(settings))
  parameters(observations)<- parameters

  return(observations)
}
