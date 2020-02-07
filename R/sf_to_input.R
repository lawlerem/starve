#' @include classes.R access_staRVe.R access_TMB_out.R formula.R
NULL

#' Print a list of implemented response distributions and link functions.
#'
#' @param which A character vector containing one or both of "distribution"
#'  and "link". Defaults to both.
#'
#' @return A named character vector giving the implemented distributions and/or
#'  link functions, depending on the value of the `which` parameter.
#'
#' @export
get_staRVe_distributions<- function(which = c("distribution","link")) {
  if( "distribution" %in% which ) {
    distributions<- c("gaussian", # 0
                      "poisson", # 1
                      "negative binomial", # 2
                      "bernoulli", # 3
                      "gamma") # 4
    names(distributions)<- rep("distribution",length(distributions))
  } else { distributions<- character(0) }
  if( "link" %in% which ) {
    links<- c("identity", # 0
              "log", # 1
              "logit") # 2
    names(links)<- rep("link",length(links))
  } else { links<- character(0) }

  return(c(distributions,links))
}

.distribution_to_code<- function(distribution) {
  distribution_code<- charmatch(distribution,get_staRVe_distributions("distribution"))
  if( is.na(distribution_code) || distribution_code == 0  ) {
    stop("Supplied distribution is not implemented, or matches multiple distributions.")
  } else {
    distribution_code<- distribution_code - 1 # Convert to cpp
  }

  return(distribution_code)
}

.link_to_code<- function(link) {
  link_code<- charmatch(link,get_staRVe_distributions("link"))
  if( is.na(link_code) || link_code == 0 ) {
    stop("Supplied link function is not implemented, or matches multiple link functions.")
  } else {
    link_code<- link_code - 1 # Convert to cpp
  }

  return(link_code)
}

.w_time_for_process<- function(Process,
                                n_time) {
  w_time<- sapply(seq(0,n_time-1),rep,nrow(Process))
  w_time<- c(w_time) # Remove names
  return(w_time)
}


#' Convert an sf object to a spatio-temporal Process, with TMB input.
#'
#' @param sf_object An object of class `sf` containing point geometries. All
#'   locations will be used for every time index.
#' @param n_neighbours A positive integer giving the (maximum) number of
#'   parents for each node.
#' @param n_time A positive integer giving the number of time indices.
#' @param time_type Which time structure should be used? Must be one of
#'    \code{independent}, \code{ar1} (default), or \code{rw}.
#' @param silent Should intermediate calculations be printed?
#' @param max_dist The maximum distance used to search for parents.
#'   Unless this has a units attribute, units are assumed to be the same as
#'   the supplied \code{distance_units}. See \code{\link{construct_dag}}.
#' @param distance_units Which units should be used for distances?
#'   See \code{\link{construct_dag}}.
#'
#' @return A list with 4 elements:
#'   \describe{
#'     \item{Process}{An sf object containing point geometries for each random
#'       effect and each time point.}
#'     \item{data}{A list to pass to TMB::MakeADFun.}
#'     \item{pars}{A list to pass to TMB::MakeADFun.}
#'     \item{rand}{A list to pass to TMB::MakeADFun.}
#'     \item{map}{A list to pass to TMB::MakeADFun.}
#'  }
.sf_to_process<- function(sf_object,
                         n_neighbours,
                         time_range,
                         time_type,
                         time_name,
                         silent = T,
                         max_dist = Inf,
                         distance_units = "km",
                         ...) {
  Process<- sf_object[,attr(sf_object,"sf_column")] # Only need locations
  Process<- unique(Process)
  Process<- order_by_location(Process)
  Process_dag<- construct_dag(Process,
                              n_neighbours = n_neighbours,
                              silent = silent,
                              max_dist = max_dist,
                              distance_units = distance_units)

  n_time<- max(time_range)-min(time_range) + 1 # Plus 1 to include year 0

  w_time<- .w_time_for_process(Process,n_time)
  proc_w<- numeric(length(w_time))

  data<- list(w_time = w_time,
              ws_edges = Process_dag[[1]],
              ws_dists = Process_dag[[2]])
  pars<- list(logtau = 0,
              logrho = log(mean(unlist(data$ws_dists))),
              logit_w_phi = 0,
              proc_w = proc_w)
  rand<- c("proc_w")
  map<- list()

  if( time_type == "ar1" ) {
    # Do nothing
  } else if ( time_type == "rw" ) {
    pars$logit_w_phi<- Inf # w_phi --> 1
    map$logit_w_phi<- factor(NA)
  } else if ( time_type == "independent" ) {
    pars$logit_w_phi<- -Inf # w_phi --> 0
    map$logit_w_phi<- factor(NA)
  } else {}

  if( n_time <= 1 ) {
    pars$logit_w_phi<- -Inf
    map$logit_w_phi<- factor(NA)
  } else { }

  Process<- do.call(rbind,lapply(seq(n_time),function(i) return(Process))) # Create n_year copies
  Process[,time_name]<- w_time+ min(time_range)

  return(list(Process = Process,
              data = data,
              pars = pars,
              rand = rand,
              map = map))
}


#' Convert an sf object to a spatio-temporal Observation.
#'
#' @param sf_object An object of class `sf` containing point geometries and
#'   covariate columns.
#' @param Process An object of class `sf` containing point geometries. These
#'   will be used as the parent nodes for the observation locations.
#' @param A formula used to describe the model. See \code{prepare_staRVe_input}.
#' @param n_neighbours A positive integer giving the (maximum) number of
#'   parents for each node.
#' @param distribution A character vector giving the response distribution.
#'   See \code{get_staRVe_distributions("distribution")}.
#' @param link A character vector giving the response link function.
#'  See \code{get_staRVe_distributions("link")}.
#' @param silent Should intermediate calculations be printed?
#' @param max_dist The maximum distance used to search for parents.
#'   Unless this has a units attribute, units are assumed to be the same as
#'   the supplied \code{distance_units}. See \code{\link{construct_dag}}.
#' @param distance_units Which units should be used for distances?
#'   See \code{\link{construct_dag}}.
#'
#' @return A list with 4 elements:
#'   \describe{
#'     \item{Observation}{An sf object containing point geometries and covariates
#'        for each observation.}
#'     \item{data}{A list to pass to TMB::MakeADFun.}
#'     \item{pars}{A list to pass to TMB::MakeADFun.}
#'     \item{rand}{A list to pass to TMB::MakeADFun.}
#'     \item{map}{A list to pass to TMB::MakeADFun.}
#'  }
.sf_to_observation<- function(sf_object,
                             Process,
                             formula,
                             n_neighbours,
                             distribution,
                             link,
                             silent = T,
                             max_dist = Inf,
                             distance_units = "km",
                             ...) {
  distribution_code<- .distribution_to_code(distribution)
  response_pars<- switch(distribution_code+1,
                        numeric(1), # normal
                        numeric(0), # Poisson
                        numeric(1), # Neg. Binom.
                        numeric(0), # Bernoulli
                        numeric(1)) # Gamma
  link_code<- .link_to_code(link)

  time_name<- attr(.time_from_formula(formula,sf_object),"name")
  Observation<- order_by_location(sf_object,time=sf_object[[time_name]])


  obs_y<- .response_from_formula(formula,Observation)
  if( !(attr(obs_y,"name") %in% names(Observation)) ) {
    Observation[,attr(y,"name")]<- c(obs_y) # Add response variable (with name) to Observation
  } else {}

  y_time<- .time_from_formula(formula,Observation)
  y_time<- y_time - min(y_time) # Starts at 0
  n_time<- max(y_time)+1 # Plus 1 to include year 0

  mean_design<- .mean_design_from_formula(formula,Observation)
  mean_parameters<- numeric(ncol(mean_design))

  Observation_dag<- construct_obs_dag(Observation,
                                      Process,
                                      n_neighbours = n_neighbours,
                                      silent = silent,
                                      max_dist = max_dist,
                                      distance_units = distance_units)
  Observation_edges<- Observation_dag[[1]]
  Observation_dists<- Observation_dag[[2]]

  edge_degrees<- unlist(lapply(Observation_edges,length))
  link_w_idx<- (edge_degrees != 1)
  link_w<- numeric(sum(link_w_idx))
  link_w_time<- y_time[link_w_idx]

  data<- list(n_time = n_time,
              distribution_code = distribution_code,
              link_code = link_code,
              y_time = c(y_time),
              obs_y = c(obs_y),
              ys_edges = Observation_edges,
              ys_dists = Observation_dists,
              link_w_time = link_w_time,
              mean_design = mean_design)
  pars<- list(mu = 0,
              response_pars = response_pars,
              mean_pars = mean_parameters,
              link_w = link_w)
  rand<- c("link_w")
  map<- list()

  obs_var_names<- c(attr(obs_y,"name"),attr(y_time,"name"),attr(Observation,"sf_column"))
  Observation<- Observation[,obs_var_names]
  Observation<- cbind(Observation,mean_design)

  return(list(Observation = Observation,
              data = data,
              pars = pars,
              rand = rand,
              map = map))
}




#' Convert an `sf' object to a form appropriate for the TMB::MakeADFun function.
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
#' The variables in the mean(...) ``special" are linear predictors for the mean
#' of the response variable. Any formula valid for the \code{lm} command can be used
#' inside the mean(...), however any missing values will likely cause errors.
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
#'
#' @return A list with three components. The first is a list of data and parameters
#'  to pass directly to TMB::MakeADFun. The second is a list of \code{sf} objects
#'  storing minimal versions of Observation (== data) and Process. The third is a list of
#'  settings used in the model (n_neighbours, distance_units, etc.).
#'
#' @export
prepare_staRVe_input<- function(formula,
                                data,
                                Process = data,
                                n_neighbours = 10,
                                distribution = "gaussian",
                                link = "identity",
                                silent = T,
                                max_dist = Inf,
                                distance_units = "km",
                                fit = F,
                                ...) {
  Observation<- data # Want to call data in the call for conformity with glm
  max_dist<- units::set_units(max_dist,distance_units,mode="standard")
  max_dist<- units::set_units(max_dist,"m") # st_nn needs input in meters.

  time_info<- .time_from_formula(formula,Observation)
  Process<- Process[,attr(Process,"sf_column")] # We only need the locations
  Process<- unique(Process)
  Process<- order_by_location(Process)
  Observation<- order_by_location(Observation,time=Observation$Year)

  tmb_proc<- .sf_to_process(Process,
                            n_neighbours = n_neighbours,
                            time_range = range(time_info),
                            time_type = attr(time_info,"type"),
                            time_name = attr(time_info,"name"),
                            silent = silent,
                            max_dist = max_dist,
                            distance_untis = distance_units,
                            ...)
  tmb_obs<- .sf_to_observation(Observation,
                               Process,
                               formula = formula,
                               n_neighbours = n_neighbours,
                               distribution = distribution,
                               link = link,
                               silent = T,
                               max_dist = max_dist,
                               distance_units = distance_units,
                               ...)

  TMB_objects<- list(data = c(tmb_obs$data,
                              tmb_proc$data),
                     pars = c(tmb_obs$pars,
                              tmb_proc$pars),
                     rand = c(tmb_obs$rand,
                              tmb_proc$rand),
                     map = c(tmb_obs$map,
                             tmb_proc$map))
  sf_objects<- list(Observation = tmb_obs$Observation,
                    Process = tmb_proc$Process)
  settings<- list(n_neighbours = n_neighbours,
                  distance_units = distance_units,
                  formula = formula,
                  distribution_code = TMB_objects$data$distribution_code,
                  link_code = TMB_objects$data$link_code,
                  time_column = attr(time_info,"name"))
  return_val<- list(TMB_objects = TMB_objects,
                    sf_objects = sf_objects,
                    settings = settings)

  if( fit == T ) {
    model_fit<- fit_staRVe(return_val,...)
    return(model_fit)
  } else {
    return(return_val)
  }

  return(return_val) # Not actually necessary but a good safeguard
}
