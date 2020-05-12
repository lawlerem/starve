#' @include classes.R generics.R staRVe_process.R staRVe_observations.R staRVe_settings.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_model} instead.
#'
#' @export
#' @rdname staRVe_model
setMethod(
  f = "initialize",
  signature = "staRVe_model",
  definition = function(.Object,
                        process = new("staRVe_process"),
                        observations = new("staRVe_observations"),
                        settings = new("staRVe_settings")) {
    process(.Object)<- process
    observations(.Object)<- observations
    settings(.Object)<- settings

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_model}.
#'
#' @param x An object of class \code{staRVe_model}.
#' @param value A replacement value
#'
#' @family Access_staRVe_model
#' @name Access_staRVe_model
NULL

#' @export
setMethod(f = "process",
          signature = "staRVe_model",
          definition = function(x) return(x@process)
)
#' @export
setReplaceMethod(f = "process",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@process<- value
  return(x)
})



#' @export
setMethod(f = "observations",
          signature = "staRVe_model",
          definition = function(x) return(x@observations)
)
#' @export
setReplaceMethod(f = "observations",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@observations<- value
  return(x)
})



#' @export
setMethod(f = "settings",
          signature = "staRVe_model",
          definition = function(x) return(x@settings)
)
#' @export
setReplaceMethod(f = "settings",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@settings<- value
  return(x)
})



###############
###         ###
### Utility ###
###         ###
###############

#' Create an object of class \code{staRVe_model}.
#'
#' .
#'
#' The formula object should always be of the form \code{y ~ mean(x+z) + time(t,type="ar1")}.
#' The variable y should be replaced with the desired response variable, and t
#' should be replaced with the desired time index. There are currently three
#' valid options for the `type' argument in \code{time(t,type="ar1")} -- "ar1" for
#' an AR(1) structure, "rw" for a random walk, and "independent" for independent
#' spatial fields each year.
#'
#' The variables in the \code{mean(...)} ``special" are linear predictors for the mean
#' of the response variable. Any formula valid for the \code{lm} command can be used
#' inside the \code{mean(...)}, however any missing values will likely cause errors.
#'
#' @param formula A formula object used to describe the model. The details are given
#'  under `Details'.
#' @param data An object of class `sf` containing point geometries. Data
#'  used in the `formula' object will be found here.
#' @param Process An object of class `sf` containing point geometries.
#'  The default value uses the same locations as the observations. All locations
#'  will be used in every time index, so special care should be taken in choosing
#'  the number of points present. Any data fields (include time) will be discarded.
#' @param n_neighbours An integer giving the number of parents for each node.
#' @param p_far_neighbours What percent of neighbours should be randomly selected?
#' @param distribution A character vector giving the response distribution. The
#'  default is "gaussian". See \code{get_staRVe_distributions("distribution")}.
#' @param link A character vector giving the response link function. The default
#'  is "identity". See \code{get_staRVe_distributions("link")}.
#' @param silent Should intermediate calculations be printed?
#' @param max_dist The maximum distance used to search for parents.
#'  Unless this has a units attribute, units are assumed to be the same as
#'  the supplied \code{distance_units}. See \code{\link{construct_dag}}.
#' @param distance_units Which units should be used for distances?
#'  See \code{\link{construct_dag}}.
#' @param fit Should the model be fit in this call? If true, only returns
#'  the fitted model.
#'
#' @return A list with three components. The first is a list of data and parameters
#'  to pass directly to TMB::MakeADFun. The second is a list of \code{sf} objects
#'  storing minimal versions of Observation (== data) and Process. The third is a list of
#'  settings used in the model (n_neighbours, distance_units, etc.).
#'
#' @export
prepare_staRVe_model<- function(formula,
                                data,
                                nodes = data,
                                n_neighbours = 10,
                                p_far_neighbours = 0.2,
                                distribution = "gaussian",
                                link = "identity",
                                silent = T,
                                max_dist = Inf,
                                distance_units = "km",
                                fit = F,
                                ...) {
  time_form<- .time_from_formula(formula,data)
  model<- new("staRVe_model")
  settings(model)<- new("staRVe_settings",
    formula = formula,
    n_neighbours = n_neighbours,
    p_far_neighbours = p_far_neighbours,
    distance_units = distance_units,
    max_distance = max_dist
  )
  process(model)<- prepare_staRVe_process(
    nodes = nodes,
    time = as.data.frame(data)[,attr(time_form,"name"),drop=F],
    settings = settings(model)
  )
  observations(model)<- prepare_staRVe_observations(
    data = data,
    process = process(model),
    settings = settings(model),
    distribution = distribution,
    link = link
  )

  return(model)
}



#' Convert a staRVe_model object to a form suitable for TMB input.
#'
#' @export
setMethod(f = "TMB_in",
          signature = "staRVe_model",
          definition = function(x) {
  process<- process(x)
  observations<- observations(x)
  time_column<- attr(random_effects(process),"time_column")

  data<- list(
    n_time = length(unique(random_effects(process)[,time_column,drop=T])),
    distribution_code = .distribution_to_code(
      response_distribution(parameters(observations))
    ),
    link_code = .link_to_code(
      link_function(parameters(observations))
    ),
    y_time = data(observations)[,time_column,drop=T],
    obs_y = c(.response_from_formula(
      formula(settings(x)),
      data(observations)
    )),
    ys_edges = edges(transient_graph(observations)),
    ys_dists = distances(transient_graph(observations)),
    resp_w_time = data(observations)[
      sapply(edges(transient_graph(observations)),length) > 1,
      time_column,
      drop=T
    ],
    mean_design = .mean_design_from_formula(
      formula(settings(x)),
      data(observations)
    ),
    covar_code = .covariance_to_code(
      covariance_function(parameters(process))
    ),
    w_time = random_effects(process)[,time_column,drop=T],
    ws_edges = edges(persistent_graph(process)),
    ws_dists = distances(persistent_graph(process)),
    pred_w_time = numeric(0),
    pred_ws_edges = list(numeric(0)),
    pred_ws_dists = list(matrix(0,nrow=0,ncol=0))
  )
  para<- list(
    mu = fixed_effects(parameters(observations))["mu","par"],
    response_pars = response_parameters(parameters(observations))[,"par"],
    mean_pars = fixed_effects(parameters(observations))[colnames(data$mean_design),"par"],
    resp_w = numeric(
      sum(sapply(edges(transient_graph(observations)),length) > 1)
    ),
    logtau = ifelse(
      spatial_parameters(parameters(process))["tau","par"] > 0,
      log(spatial_parameters(parameters(process))["tau","par"]),
      log(1)
    ),
    logrho = ifelse(
      spatial_parameters(parameters(process))["rho","par"] > 0,
      log(spatial_parameters(parameters(process))["rho","par"]),
      log(1)
    ),
    lognu = ifelse(
      spatial_parameters(parameters(process))["nu","par"] > 0,
      log(spatial_parameters(parameters(process))["nu","par"]),
      log(0.5)
    ),
    logit_w_phi = ifelse(
      time_parameters(parameters(process))["phi","par"] >= 0
        && time_parameters(parameters(process))["phi","par"] <= 1,
      qlogis(time_parameters(parameters(process))["phi","par"]),
      qlogis(0)
    ),
    proc_w = random_effects(process)[,"w",drop=T],
    pred_w = numeric(0)
  )
  rand<- c("resp_w","proc_w","pred_w")
  map<- list()

  return(list(
    data = data,
    para = para,
    rand = rand,
    map = map
  ))
})
