#' @include classes.R generics.R staRVe_process.R staRVe_observations.R staRVe_parameters.R staRVe_settings.R
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
#' @noRd
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
#' @family access_staRVe_model
#' @name access_staRVe_model
NULL

#' @export
#' @rdname access_staRVe_model
setMethod(f = "process",
          signature = "staRVe_model",
          definition = function(x) return(x@process)
)
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "process",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@process<- value
  return(x)
})



#' @export
#' @rdname access_staRVe_model
setMethod(f = "observations",
          signature = "staRVe_model",
          definition = function(x) return(x@observations)
)
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "observations",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@observations<- value
  return(x)
})



#' @export
#' @rdname access_staRVe_model
setMethod(f = "settings",
          signature = "staRVe_model",
          definition = function(x) return(x@settings)
)
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "settings",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@settings<- value
  return(x)
})



###################
### Meta-Access ###
###################

#' @export
#' @rdname access_staRVe_model
setMethod(f = "parameters",
          signature = "staRVe_model",
          definition = function(x) {
  parameters<- new("staRVe_parameters",
                   process_parameters = parameters(process(x)),
                   observation_parameters = parameters(observations(x)))
  return(parameters)
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "parameters",
                 signature = c("staRVe_model","staRVe_parameters"),
                 definition = function(x,value) {
  parameters(process(x))<- as(value,"staRVe_process_parameters")
  parameters(observations(x))<- as(value,"staRVe_observation_parameters")
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "dat",
          signature = "staRVe_model",
          definition = function(x) {
  return(dat(observations(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "dat",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  dat(observations(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "random_effects",
          signature = "staRVe_model",
          definition = function(x) {
  return(random_effects(process(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "random_effects",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  random_effects(process(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "graph",
          signature = "staRVe_model",
          definition = function(x) {
  graph<- list(persistent_graph = persistent_graph(process(x)),
               transient_graph = transient_graph(observations(x)))
  return(graph)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "covariance_function",
          signature = "staRVe_model",
          definition = function(x) {
  return(covariance_function(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "covariance_function",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  covariance_function(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "spatial_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(spatial_parmaeters(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "spatial_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  spatial_parameters(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "spatial_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(spatial_parameters(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "spatial_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  spatial_parameters(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "time_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(time_parameters(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "time_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  time_parameters(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "response_distribution",
          signature = "staRVe_model",
          definition = function(x) {
  return(response_distribution(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "response_distribution",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  response_distribution(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "response_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(response_parameters(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "response_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  response_parameters(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "link_function",
          signature = "staRVe_model",
          definition = function(x) {
  return(link_function(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "link_function",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  link_function(parameters(x))<- value
  return(x)
})

#' @export
#' @rdname access_staRVe_model
setMethod(f = "fixed_effects",
          signature = "staRVe_model",
          definition = function(x) {
  return(fixed_effects(parameters(x)))
})
#' @export
#' @rdname access_staRVe_model
setReplaceMethod(f = "fixed_effects",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  fixed_effects(parameters(x))<- value
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
#' The formula object should always be of the form
#'   \code{y ~ sample.size(n)+mean(x+z) + time(t,type="ar1") + space("matern",nu=1.5)}.
#' The variable y should be replaced with the desired response variable, and t
#' should be replaced with the desired time index. There are currently three
#' valid options for the `type' argument in \code{time(t,type="ar1")} -- "ar1" for
#' an AR(1) structure, "rw" for a random walk, and "independent" for independent
#' spatial fields each year. The \code{space(...)} term specifies the spatial covariance
#' function.
#'
#' The sample.size(...) term is only used if the response distribution is \code{binomial} or \code{atLeastOneBinomial}. If it is missing the sample sizes are assumed to all be 1.
#'
#' If the \code{time(...)} term is missing, all observations are assumed to be
#' at the same time. If the \code{space(...)} term is missing, the spatial
#' covariance function defaults to the exponential function.
#'
#' The variables in the \code{mean(...)} ``special" are linear predictors for the mean
#' of the response variable. Any formula valid for the \code{lm} command can be used
#' inside the \code{mean(...)}, however any missing values will likely cause errors.
#'
#' @param formula A formula object used to describe the model. The details are given
#'  under `Details'.
#' @param data An object of class `sf` containing point geometries. Data
#'  used for the `formula' object will be found here.
#' @param nodes An object of class `sf` containing point geometries.
#'  The default value uses the same locations as the observations. These
#'  locations will be used as the nodes for the random effects. All locations
#'  will be used for each year.
#' @param n_neighbours An integer giving the number of parents for each node.
#' @param p_far_neighbours What percent of neighbours should be randomly selected?
#' @param distribution A character vector giving the response distribution. The
#'  default is "gaussian". The "atLeastOneBinomial" distribution models the probability of at least one success in n trials. See \code{get_staRVe_distributions("distribution")}.
#' @param link A character vector giving the response link function. The default
#'  is "identity". See \code{get_staRVe_distributions("link")}.
#' @param silent Should intermediate calculations be printed?
#' @param max_dist The maximum distance used to search for parents.
#'  Unless this has a units attribute, units are assumed to be the same as
#'  the supplied \code{distance_units}.
#' @param distance_units Which units should be used for distances?
#' @param fit Should the model be fit in this call? If true, returns a fitted model.
#'
#' @return A staRVe_model object. If fit=T, a staRVe_fit object.
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

  if( fit == T ) {
    model<- staRVe_fit(model,silent = silent,...)
  } else {}

  return(model)
}



#' Convert a staRVe_model object to a form suitable for TMB input.
#'
#' @noRd
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
    y_time = c(dat(observations)[,time_column,drop=T]),
    obs_y = c(.response_from_formula(
      formula(settings(x)),
      dat(observations)
    )),
    ys_edges = edges(idxR_to_C(transient_graph(observations))),
    ys_dists = distances(transient_graph(observations)),
    resp_w_time = c(dat(observations)[
      sapply(edges(transient_graph(observations)),length) > 1,
      time_column,
      drop=T
    ]),
    mean_design = .mean_design_from_formula(
      formula(settings(x)),
      dat(observations)
    ),
    sample_size = .sample_size_from_formula(
      formula(settings(x)),
      dat(observations),
      nullReturn = T
    )[,1],
    covar_code = .covariance_to_code(
      covariance_function(parameters(process))
    ),
    w_time = c(random_effects(process)[,time_column,drop=T]),
    ws_edges = edges(idxR_to_C(persistent_graph(process))),
    ws_dists = distances(persistent_graph(process)),
    pred_w_time = numeric(0),
    pred_ws_edges = list(numeric(0)),
    pred_ws_dists = list(matrix(0,nrow=0,ncol=0))
  )

  time_names<- c(
    "y_time",
    "resp_w_time",
    "w_time",
    "pred_w_time"
  )
  data[time_names]<- lapply(data[time_names],function(x) {
    return(x - min(data$w_time))
  })

  para<- list(
    mu = fixed_effects(parameters(observations))["mu","par"],
    # response_pars = response_parameters(parameters(observations))[,"par"],
    working_response_pars = switch(response_distribution(parameters(observations)),
      gaussian = ifelse( # Normal; std. dev.
        response_parameters(parameters(observations))["sd","par"] > 0 ||
          response_parameters(parameters(observations))["sd","fixed"] == T,
        log(response_parameters(parameters(observations))["sd","par"]),
        log(1)
      ),
      poisson = numeric(0), # Poisson; NA
      `negative binomial` = ifelse( # Neg. Binom.; overdispersion
        response_parameters(parameters(observations))["overdispersion","par"] >= 1 ||
          reponse_parameters(parameters(observations))["overdispersion","fixed"] == T,
        log(response_parameters(parameters(observations))["overdispersion","par"]-1),
        log(1)
      ),
      bernoulli = numeric(0), # Bernoulli; NA
      gamma = ifelse( # Gamma; std. dev.
        response_parameters(parameters(observations))["sd","par"] > 0 ||
          response_parameters(parameters(observations))["sd","fixed"] == T,
        log(response_parameters(parameters(observations))["sd","par"]),
        log(1)
      ), # Gamma; std. dev.
      lognormal = ifelse( # Log-Normal; std. dev.
        response_parameters(parameters(observations))["sd","par"] > 0 ||
          response_parameters(parameters(observations))["sd","fixed"] == T,
        log(response_parameters(parameters(observations))["sd","par"]),
        log(1)
      ), # Log-normal; std. dev.
      binomial = numeric(0), # Binomial; NA
      atLeastOneBinomial = numeric(0)# atLeastOneBinomial; NA
    ),
    mean_pars = fixed_effects(parameters(observations))[colnames(data$mean_design),"par"],
    resp_w = numeric(
      sum(sapply(edges(transient_graph(observations)),length) > 1)
    ),
    logScaleTau = ifelse(
      spatial_parameters(parameters(process))["scaleTau","par"] > 0 ||
        spatial_parameters(parameters(process))["scaleTau","fixed"] == T,
      log(spatial_parameters(parameters(process))["scaleTau","par"]),
      log(1)
    ),
    logrho = ifelse(
      spatial_parameters(parameters(process))["rho","par"] > 0 ||
        spatial_parameters(parameters(process))["rho","fixed"] == T,
      log(spatial_parameters(parameters(process))["rho","par"]),
      log(1)
    ),
    lognu = ifelse(
      spatial_parameters(parameters(process))["nu","par"] > 0 ||
        spatial_parameters(parameters(process))["nu","fixed"] == T,
      log(spatial_parameters(parameters(process))["nu","par"]),
      log(0.5)
    ),
    logit_w_phi = ifelse(
      (time_parameters(parameters(process))["phi","par"] >= 0
        && time_parameters(parameters(process))["phi","par"] <= 1) ||
        time_parameters(parameters(process))["phi","fixed"] == T,
      qlogis(time_parameters(parameters(process))["phi","par"]),
      qlogis(0)
    ),
    proc_w = c(random_effects(process)[,"w",drop=T]),
    pred_w = numeric(0)
  )
  rand<- c("resp_w","proc_w","pred_w")
  map<- list(
    mu = fixed_effects(parameters(observations))["mu","fixed"],
    working_response_pars = response_parameters(parameters(observations))[,"fixed"],
    mean_pars = fixed_effects(parameters(observations))[colnames(data$mean_design),"fixed"],
    resp_w = logical(
      sum(sapply(edges(transient_graph(observations)),length) > 1)
    ),
    logScaleTau = spatial_parameters(parameters(process))["scaleTau","fixed"],
    logrho = spatial_parameters(parameters(process))["rho","fixed"],
    lognu = spatial_parameters(parameters(process))["nu","fixed"],
    logit_w_phi = time_parameters(parameters(process))["phi","fixed"],
    proc_w = random_effects(process)[,"fixed",drop=T],
    pred_w = logical(0)
  )
  map<- lapply(map,.logical_to_map)

  return(list(
    data = data,
    para = para,
    rand = rand,
    map = map
  ))
})


#' @noRd
setMethod(f = "update_staRVe_model",
          signature = c(x = "staRVe_model",
                        y = "TMB_out"),
          definition = function(x,y) {
  sdr_mat<- summary(sdr(y))
  par_names<- character(0)

  spatial_parameters(parameters(process(x)))<- within(
    spatial_parameters(parameters(process(x))),{
      par_names<<- c("par_rho","par_scaleTau","par_nu")
      par<- sdr_mat[par_names,1]
      se<- sdr_mat[par_names,2]
    }
  )

  time_parameters(parameters(process(x)))<- within(
    time_parameters(parameters(process(x))),{
      par_names<<- c("par_w_phi")
      par<- sdr_mat[par_names,1]
      se<- sdr_mat[par_names,2]
    }
  )

  random_effects(process(x))<- within(
    random_effects(process(x)),{
      par_names<<- c("proc_w")
      w<- sdr_mat[rownames(sdr_mat) %in% par_names,1]
      se<- sdr_mat[rownames(sdr_mat) %in% par_names,2]
    }
  )

  fixed_effects(parameters(observations(x)))<- within(
    fixed_effects(parameters(observations(x))),{
      par_names<<- c("par_mu","par_mean_pars")
      par<- c(sdr_mat[par_names[[1]],1],
              sdr_mat[rownames(sdr_mat) %in% par_names[[2]],1])
      se<- c(sdr_mat[par_names[[1]],2],
              sdr_mat[rownames(sdr_mat) %in% par_names[[2]],2])
    }
  )

  response_parameters(parameters(observations(x)))<- within(
    response_parameters(parameters(observations(x))),{
      par_names<<- c("par_sd","par_overdispersion")
      par<- sdr_mat[rownames(sdr_mat) %in% par_names,1]
      se<- sdr_mat[rownames(sdr_mat) %in% par_names,2]
    }
  )

  data<- dat(observations(x))
  obs_dag<- transient_graph(observations(x))
  random_effects<- random_effects(process(x))
  time_column<- attr(data,"time_column")

  resp_w_idx<- 1
  resp_w<- sdr_mat[rownames(sdr_mat) == "resp_w",]

  for( i in seq(nrow(data)) ) {
    if( length(edges(obs_dag)[[i]]) == 1 ) {
      w<- random_effects[
        random_effects[,time_column,drop=T] == data[i,time_column,drop=T],
      ]
      data[i,c("w","w_se")]<- as.data.frame(w)[edges(obs_dag)[[i]],c("w","se")]
    } else {
      data[i,c("w","w_se")]<- resp_w[resp_w_idx,]
      resp_w_idx<- resp_w_idx+1
    }
  }
  dat(observations(x))<- data

  return(x)
})
