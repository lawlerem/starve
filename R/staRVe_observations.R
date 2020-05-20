#' @include classes.R generics.R dag.R staRVe_observation_parameters.R staRVe_process.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

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
    if( !is.null(attr(data(.Object),"active_time")) &&
        "time" %in% colnames(data(.Object)) ) {
      attr(data(.Object),"active_time")<- "time"
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

#' Get or set slots from an object of class \code{staRVe_observations}.
#'
#' @param x An object of class \code{staRVe_observations}.
#' @param value A replacement value
#'
#' @family Access_staRVe_observations
#' @name Access_staRVe_observations
NULL

#' @export
setMethod(f = "data",
          signature = "staRVe_observations",
          definition = function(x) return(x@data)
)
#' @export
setReplaceMethod(f = "data",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@data<- value
  return(x)
})



#' @export
setMethod(f = "transient_graph",
          signature = "staRVe_observations",
          definition = function(x) return(x@transient_graph)
)
#' @export
setReplaceMethod(f = "transient_graph",
                 signature = "staRVe_observations",
                 definition = function(x,value) {
  x@transient_graph<- value
  return(x)
})



#' @export
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

prepare_staRVe_observations<- function(data,
                                       process,
                                       settings = new("staRVe_settings"),
                                       distribution = "gaussian",
                                       link = "identity") {
  observations<- new("staRVe_observations")
  time_form<- .time_from_formula(formula(settings),data)

  # data = "sf"
  data<- order_by_location(data,time = data[[attr(time_form,"name")]])
  y<- .response_from_formula(formula(settings),data)
  time_form<- .time_from_formula(formula(settings),data) #in order)
  response<- data.frame(y = c(y),t = c(time_form))
  names(response)<- c(attr(y,"name"),attr(time_form,"name"))

  design<- .mean_design_from_formula(formula(settings),data,return = "model.frame")

  data(observations)<- sf:::cbind.sf(
    w = 0,
    w_se = NA,
    linear = NA,
    linear_se = NA,
    response = NA,
    response_se = NA,
    design,
    response,
    data[,attr(data,"sf_column")]
  )
  attr(data(observations),"time_column")<- attr(time_form,"name")



  # transient_graph = "dag"
  transient_graph(observations)<- construct_obs_dag(
    x = data,
    y = random_effects(process),
    settings = new("staRVe_settings"),
  )



  # parameters = "staRVe_observation_parameters"
  parameters<- new("staRVe_observation_parameters")

  response_distribution(parameters)<- unname(
    get_staRVe_distributions("distribution")[
      charmatch(distribution,get_staRVe_distributions("distribution"))
    ]
  )

  response_parameters(parameters)<- data.frame(
    par = c(switch(distribution,
      gaussian = numeric(1),
      poisson = numeric(0),
      `negative binomial` = numeric(1),
      bernoulli = numeric(0),
      gamma = numeric(1),
      lognormal = numeric(1)
    )),
    se = c(switch(distribution,
      gaussian = NA,
      poisson = numeric(0),
      `negative binomial` = NA,
      bernoulli = numeric(0),
      gamma = NA,
      lognormal = NA
    )),
    fixed = c(switch(distribution,
      gaussian = rep(F,1),
      poisson = rep(F,0),
      `negative binomial` = rep(F,1),
      bernoulli = rep(F,0),
      gamma = rep(F,1),
      lognormal = rep(F,1)
    )),
    row.names = c(switch(distribution,
      gaussian = c("sd"),
      poisson = c(),
      `negative binomial` = c("overdispersion"),
      bernoulli = c(),
      gamma = c("sd"),
      lognormal = c("sd"),
    ))
  )

  link_function(parameters)<- unname(
    get_staRVe_distributions("link")[
      charmatch(link,get_staRVe_distributions("link"))
    ]
  )

  design<- .mean_design_from_formula(formula(settings),data)
  fixed_effects(parameters)<- data.frame(
    par = c(0,numeric(ncol(design))),
    se = NA,
    fixed = rep(F,1+ncol(design)),
    row.names = c("mu",colnames(design))
  )
  parameters(observations)<- parameters

  return(observations)
}
