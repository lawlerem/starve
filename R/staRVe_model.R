#' @include classes.R getset.R generics.R staRVe_process.R staRVe_observations.R staRVe_parameters.R staRVe_settings.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param process A staRVe_process object
#' @param observations A staRVe_observations object
#' @param settings A staRVe_settings object
#'
#' @rdname staRVe-construct
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

#' Get/set process (staRVe_process)
#'
#' @param x An object
#'
#' @noRd
setMethod(f = "process",
          signature = "staRVe_model",
          definition = function(x) return(x@process)
)
setReplaceMethod(f = "process",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@process<- value
  return(x)
})



#' Get/set observations (staRVe_observations)
#'
#' @param x An object
#'
#' @noRd
setMethod(f = "observations",
          signature = "staRVe_model",
          definition = function(x) return(x@observations)
)
setReplaceMethod(f = "observations",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@observations<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get model settings
setMethod(f = "settings",
          signature = "staRVe_model",
          definition = function(x) return(x@settings)
)
setReplaceMethod(f = "settings",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  x@settings<- value
  return(x)
})



###################
### Meta-Access ###
###################


### From staRVe_process

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get temporal random effects
setMethod(f = "time_effects",
          signature = "staRVe_model",
          definition = function(x) {
  return(time_effects(process(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set temporal random effects
setReplaceMethod(f = "time_effects",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  time_effects(process(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get persistent graph random effects
setMethod(f = "pg_re",
          signature = "staRVe_model",
          definition = function(x) {
  return(pg_re(process(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set spatio-temporal random effects
setReplaceMethod(f = "pg_re",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  pg_re(process(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get transient graph random effects
setMethod(f = "tg_re",
          signature = "staRVe_model",
          definition = function(x) {
  return(tg_re(process(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set transient graph random effects
setReplaceMethod(f = "tg_re",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  tg_re(process(x))<- value
  return(x)
})




#' Get/set persistent graph
#'
#' @param x An object
#'
#' @noRd
setMethod(f = "persistent_graph",
          signature = "staRVe_model",
          definition = function(x) {
  return(persistent_graph(process(x)))
})
setReplaceMethod(f = "persistent_graph",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  persistent_graph(process(x))<- value
  return(x)
})


#' Get/set persistent graph
#'
#' @param x An object
#'
#' @noRd
setMethod(f = "transient_graph",
          signature = "staRVe_model",
          definition = function(x) {
  return(transient_graph(process(x)))
})
setReplaceMethod(f = "transient_graph",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  transient_graph(process(x))<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get a list containing the persistent and transient graphs
setMethod(f = "graph",
          signature = "staRVe_model",
          definition = function(x) {
  graph<- list(persistent_graph = persistent_graph(x),
               transient_graph = transient_graph(x))
  return(graph)
})



### From staRVe_process_parameters

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get covariance function
setMethod(f = "covariance_function",
          signature = "staRVe_model",
          definition = function(x) {
  return(covariance_function(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set covariance function. Run
#'   get_staRVe_distributions("covariance") for valid covariance functions.
#'   Setting the covariance function also overwrites the spatial parameters.
setReplaceMethod(f = "covariance_function",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  covariance_function(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get spatial parameters
setMethod(f = "spatial_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(spatial_parameters(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Get spatial parameters
setReplaceMethod(f = "spatial_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  spatial_parameters(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get time parameters
setMethod(f = "time_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(time_parameters(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set time parameters
setReplaceMethod(f = "time_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  time_parameters(parameters(x))<- value
  return(x)
})



### From staRVe_observations

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get data
setMethod(f = "dat",
          signature = "staRVe_model",
          definition = function(x) {
  return(dat(observations(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set data
setReplaceMethod(f = "dat",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  dat(observations(x))<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get data predictions
setMethod(f = "data_predictions",
          signature = "staRVe_model",
          definition = function(x) {
  return(data_predictions(observations(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set data predictions
setReplaceMethod(f = "data_predictions",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  data_predictions(observations(x))<- value
  return(x)
})




### From staRVe_observation_parameters

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get the response distribution.
setMethod(f = "response_distribution",
          signature = "staRVe_model",
          definition = function(x) {
  return(response_distribution(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set the response distribution. Run
#'   get_staRVe_distributions("distribution") for valid options.
#'   Setting the response distribution also overwrites the response
#'   parameters and link function.
setReplaceMethod(f = "response_distribution",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  response_distribution(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get response parameters
setMethod(f = "response_parameters",
          signature = "staRVe_model",
          definition = function(x) {
  return(response_parameters(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set response parameters
setReplaceMethod(f = "response_parameters",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  response_parameters(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get the link function
setMethod(f = "link_function",
          signature = "staRVe_model",
          definition = function(x) {
  return(link_function(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set the link function. Run
#'   get_staRVe_distributions("link") for valid covariance functions.
setReplaceMethod(f = "link_function",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  link_function(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get fixed effects
setMethod(f = "fixed_effects",
          signature = "staRVe_model",
          definition = function(x) {
  return(fixed_effects(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set fixed effects
setReplaceMethod(f = "fixed_effects",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  fixed_effects(parameters(x))<- value
  return(x)
})


### From staRVe_settings

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get distance units used for the model
setMethod(f = "distance_units",
          signature = "staRVe_model",
          definition = function(x) {
  return(distance_units(settings(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set distance units used for the model
setReplaceMethod(f = "distance_units",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  ranges<- lapply(spatial_parameters(x),function(sp) {
    units::set_units(sp["range","par"],
                     distance_units(x),
                     mode="standard")
  })
  distance_units(settings(x))<- value
  distance_units(persistent_graph(process(x)))<- value
  distance_units(transient_graph(observations(x)))<- value
  ranges<- lapply(ranges,units::set_units,value,mode="standard")
  for( i in seq_along(ranges) ) {
    spatial_parameters(x)[[i]]["range","par"]<- units::drop_units(ranges[[i]])
  }
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get formula used for model
setMethod(f = "formula",
          signature = "staRVe_model",
          definition = function(x) {
  return(formula(settings(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set formula used for the model
setReplaceMethod(f = "formula",
                 signature = "staRVe_model",
                 definition = function(x,value) {
  if( !all(all.vars(value) %in% colnames(dat(x))) ) {
    stop("Not changing formula. Some variables present in new formula which are not available in dat(x)")
  } else {}
  formula(settings(x))<- value

  design<- .mean_design_from_formula(value,dat(x))
  fe<- lapply(fixed_effects(x),function(v) {
    data.frame(
      par = numeric(ncol(design)),
      se = rep(NA,ncol(design)),
      fixed = rep(FALSE,ncol(design)),
      row.names = colnames(design)
    )
  })
  try(names(fe)<- names(fixed_effects(x)))
  fixed_effects(x)<- fe

  return(x)
})


setMethod(f = ".time_name",
          signature = "staRVe_model",
          definition = function(x) {
  return(.time_name(settings(x)))
})


### Extras

#' @param x An object
#'
#' @export
#' @describeIn staRVe_model Get parameters
setMethod(f = "parameters",
          signature = "staRVe_model",
          definition = function(x) {
  parameters<- new("staRVe_parameters",
                   process_parameters = parameters(process(x)),
                   observation_parameters = parameters(observations(x)))
  return(parameters)
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_model Set parameters
setReplaceMethod(f = "parameters",
                 signature = c("staRVe_model","staRVe_parameters"),
                 definition = function(x,value) {
  parameters(process(x))<- as(value,"staRVe_process_parameters")
  parameters(observations(x))<- as(value,"staRVe_observation_parameters")
  return(x)
})




###############
###         ###
### Utility ###
###         ###
###############

#' Create an object of class \code{staRVe_model}.
#'
#' 'prepare_staRVe_model' is used to take an existing `simple features` data.frame
#'    with point geometries, time information, covariates, and a response variable
#'    and perform all of the pre-processing steps necessary to fit a model with the
#'    \code{staRVe_fit} function.
#'
#' The formula object should always be of the form
#'   \code{y ~ sample.size(n)+mean(x+z) + time(t,type="ar1") + space("matern",nu=1.5)}.
#'
#' The variable y should be replaced with the desired response variable.
#'
#' The sample.size(...) term is only used if the response distribution is
#' \code{binomial}, \code{atLeastOneBinomial}, or \code{tweedie}.
#' If it is missing the sample sizes are assumed to all be 1.
#'
#' The variables in the \code{mean(...)} term are linear predictors for the mean
#' of the response variable. Any formula valid for the \code{lm} command can be used
#' inside the \code{mean(...)}, however any missing values will likely cause errors.
#' If the \code{mean(...)} term is missing, no covariates will be used.
#'
#' The \code{time(...)} term indicates which column, if any, holds the time index.
#' The variable t should be replaced with the desired time index. There are currently
#' three valid options for the `type' argument in \code{time(t,type="ar1")} --
#' "ar1" for an AR(1) structure, "rw" for a random walk, and "independent" for
#' independent spatial fields each year. If the \code{time(...)} term is missing,
#' all observations are assumed to be at the same time and a purely spatial model
#' is used.
#'
#' The \code{space(...)} term specifies the spatial covariance function. See
#' \code{get_staRVe_distributions("covariance")} for valid names to supply.
#' If using the "matern" option you can supply a value for the smoothness
#' parameter nu, which will be held constant in model fitting. If nu is not given,
#' then it will be freely estimated in the model. If the \code{space(...)} term
#' as a whole is missing, an exponential covariance function is assumed.
#'
#' @param formula A formula object used to describe the model including covariate
#'   effects, temporal effects, and the spatial covariance function. Details are
#'   given later in the `Details' section.
#' @param data An object of class `sf` containing point geometries. Data
#'  used for the `formula' object will be found here.
#' @param nodes Either an object of class `sf` containing point geometries, or
#'  an inla.mesh object (e.g.~from the output of INLA::inla.mesh.2d).
#'  The default value uses the same locations as the observations. The locations
#'  will be used as the nodes for the random effects. All locations will be used
#'  for each year. If an inla.mesh is supplied, the edges of the mesh are used
#'  to create the persistent graph.
#' @param n_neighbours An integer giving the (maximum) number of parents for each node.
#' @param p_far_neighbours What percent of neighbours should be randomly selected?
#' @param persistent_graph If an object of class \code{dag} is supplied, that
#'   graph is used for the persistent graph.
#' @param transient_graph If an object of class \code{dag} is supplied, that
#'   graph is used for the transient graph.
#' @param distribution Which response distribution to use. See
#'   \code{get_staRVe_distributions}.
#' @param link A character vector giving the response link function. See
#'   \code{get_staRVe_distributions}. If not supplied a default option is used
#'   based on the response distribution.
#' @param silent Should intermediate calculations be printed?
#' @param max_dist The maximum distance used to search for parents.
#'  Unless this has a units attribute, units are assumed to be the same as
#'  the supplied \code{distance_units}.
#' @param distance_units Which units should be used for distances?
#' @param fit Should the model be fit in this call? If true, returns a fitted model.
#' @param ... Extra options to pass to staRVe_fit if fit=TRUE
#'
#' @return A staRVe_model object. If fit=TRUE, a staRVe_fit object.
#'
#' @export
prepare_staRVe_model<- function(formula,
                                data,
                                nodes = data,
                                n_neighbours = 15,
                                p_far_neighbours = 0,
                                persistent_graph = NA,
                                transient_graph = NA,
                                distribution = "gaussian",
                                link = "default",
                                silent = TRUE,
                                max_dist = Inf,
                                distance_units = "km",
                                fit = FALSE,
                                ...) {
  # Need to find name of time column
  time_form<- .time_from_formula(formula,data)
  model<- new("staRVe_model")

  # Set the settings in the model
  settings(model)<- new("staRVe_settings",
    formula = formula,
    n_neighbours = n_neighbours,
    p_far_neighbours = p_far_neighbours,
    distance_units = distance_units,
    max_distance = max_dist
  )

  # Set up the staRVe_process
  process(model)<- prepare_staRVe_process(
    data = data,
    nodes = nodes,
    persistent_graph = persistent_graph,
    settings = settings(model)
  )

  # Set up the staRVe_observations
  observations(model)<- prepare_staRVe_observations(
    data = data,
    process = process(model),
    settings = settings(model),
    distribution = distribution,
    link = link
  )

  if( fit ) {
    model<- staRVe_fit(model,silent = silent,...)
  } else {}

  return(model)
}



#' Convert a staRVe_model object to a form suitable for TMB input.
#'
#' @return A list with elements data, para, map, and rand to supply to TMB::MakeADFun
#'
#' @noRd
setMethod(f = "TMB_in",
          signature = "staRVe_model",
          definition = function(x) {
  min_t<- min(stars::st_get_dimension_values(pg_re(x),.time_name(x)))
  data<- list(
    model = "staRVe_model",
    conditional_sim = FALSE,
    pg_edges = edges(idxR_to_C(persistent_graph(x))),
    pg_dists = distances(persistent_graph(x)),
    tg_t = .time_from_formula(formula(settings(x)),locations(tg_re(x)))[[1]]-min_t,
    tg_edges = edges(idxR_to_C(transient_graph(x))),
    tg_dists = distances(transient_graph(x)),
    cv_code = .covariance_to_code(covariance_function(x)),
    distribution_code = .distribution_to_code(response_distribution(x)),
    link_code = .link_to_code(link_function(x)),
    obs = as.matrix(.response_from_formula(formula(x),dat(x))),
    idx = cbind(
      dat(x)$graph_idx-1,
      .time_from_formula(formula(x),dat(x))[[1]]-min_t
    ),
    sample_size = as.matrix(.sample_size_from_formula(formula(x),dat(x),unique_vars=FALSE)),
    mean_design = as.matrix(.mean_design_from_formula(formula(x),dat(x),"model.matrix"))
  )
  para<- list(
    ts_re = time_effects(x)[["w"]],
    working_ts_pars = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        c(time_parameters(x)[[v]]["mu","par"],
          ifelse( # -1 <= ar1 <= +1
              (time_parameters(x)[[v]]["ar1","par"] >= -1
                && time_parameters(x)[[v]]["ar1","par"] <= 1) ||
              time_parameters(x)[[v]]["ar1","fixed"] == TRUE,
            qlogis(0.5*(1+time_parameters(x)[[v]]["ar1","par"])),
            qlogis(0.5*(1+0))
          ),
          ifelse( # sd > 0
              time_parameters(x)[[v]]["sd","par"] > 0 ||
              time_parameters(x)[[v]]["sd","fixed"] == TRUE,
            log(time_parameters(x)[[v]]["sd","par"]),
            log(1)
          )
        )
      })
    ),
    pg_re = pg_re(x)[["w"]],
    tg_re = (if(nrow(locations(tg_re(x))) == 0) {
        array(0,dim=c(0,staRVe:::.n_response(formula(x))))
      } else {
        values(tg_re(x))[["w"]]
      }),
    working_cv_pars = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        switch(covariance_function(x)[[v]],
          c(# Default -- all Matern-type covariance functions (exponential, gaussian, etc)
            ifelse( # std. dev. > 0
                spatial_parameters(x)[[v]]["sd","par"] > 0 ||
                spatial_parameters(x)[[v]]["sd","fixed"] == TRUE,
              log(spatial_parameters(x)[[v]]["sd","par"]),
              log(1)
            ),
            ifelse( # rho > 0
              spatial_parameters(x)[[v]]["range","par"] > 0 ||
              spatial_parameters(x)[[v]]["range","fixed"] == TRUE,
              log(spatial_parameters(x)[[v]]["range","par"]),
              log(100*mean(do.call(c,distances(graph(x)$persistent_graph))))
            ),
            ifelse( # nu > 0
                spatial_parameters(x)[[v]]["nu","par"] > 0 ||
                spatial_parameters(x)[[v]]["nu","fixed"] == TRUE,
              log(spatial_parameters(x)[[v]]["nu","par"]),
              log(0.5)
            )
          )
        )}
      )
    ),
    working_response_pars = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        switch(response_distribution(x)[[v]],
          gaussian = ifelse( # Normal; std. dev. > 0
                response_parameters(x)[[v]]["sd","par"] > 0 ||
                response_parameters(x)[[v]]["sd","fixed"] == TRUE,
              log(response_parameters(x)[[v]]["sd","par"]),
              log(1)
            ),
          poisson = numeric(0), # Poisson; NA
          `negative binomial` = ifelse( # Neg. Binom.; overdispersion >= 1
                response_parameters(x)[[v]]["overdispersion","par"] >= 1 ||
                response_parameters(x)[[v]]["overdispersion","fixed"] == TRUE,
              log(response_parameters(x)[[v]]["overdispersion","par"]-1),
              log(1)
            ),
          bernoulli = numeric(0), # Bernoulli; NA
          gamma = ifelse( # Gamma; std. dev. > 0
                response_parameters(x)[[v]]["sd","par"] > 0 ||
                response_parameters(x)[[v]]["sd","fixed"] == TRUE,
              log(response_parameters(x)[[v]]["sd","par"]),
              log(1)
            ), # Gamma; std. dev.
          lognormal = ifelse( # Log-Normal; std. dev. > 0
                response_parameters(x)[[v]]["sd","par"] > 0 ||
                response_parameters(x)[[v]]["sd","fixed"] == TRUE,
              log(response_parameters(x)[[v]]["sd","par"]),
              log(1)
            ), # Log-normal; std. dev.
          binomial = numeric(0), # Binomial; NA
          atLeastOneBinomial = numeric(0), # atLeastOneBinomial; NA
          compois = ifelse( # Conway-Maxwell-Poisson; dispersion > 0
                response_parameters(x)[[v]]["dispersion","par"] > 0 ||
                response_parameters(x)[[v]]["dispersion","fixed"] == TRUE,
              -log(response_parameters(x)[[v]]["dispersion","par"]),
              # negative sign since we want >0 to be over-dispersion
              -log(1)
            ),
          tweedie = c( # tweedie
            ifelse( # dispersion>0
              response_parameters(x)[[v]]["dispersion","par"] > 0 ||
              response_parameters(x)[[v]]["dispersion","fixed"] == TRUE,
              log(response_parameters(x)[[v]]["dispersion","par"]),
              log(1)
            ),
            ifelse( # 1<power<2
              (0 < response_parameters(x)[[v]]["power","par"] &&
               response_parameters(x)[[v]]["power","par"] < 1) ||
              response_parameters(x)[[v]]["power","par"] == TRUE,
              qlogis(response_parameters(x)[[v]]["power","par"]-1),
              qlogis(1.5-1)
            )
          )
        )}
      )
    ),
    beta = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        fixed_effects(x)[[v]][colnames(data$mean_design),"par"]
      })
    )
  )
  rand<- c("ts_re","pg_re","tg_re")
  map<- list(
    working_ts_pars = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        time_parameters(x)[[v]][,"fixed"]
      })
    ),
    working_cv_pars = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        spatial_parameters(x)[[v]][,"fixed"]
      })
    ),
    working_response_pars = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        switch((nrow(response_parameters(x)[[v]]) > 0)+1,
          logical(0),
          response_parameters(x)[[v]][,"fixed"]
        )
      })
    ),
    beta = do.call(.cbind_no_recycle,
      lapply(seq(.n_response(formula(x))),function(v) {
        fixed_effects(x)[[v]][colnames(data$mean_design),"fixed"]
      })
    )
  )
  map<- lapply(map,.logical_to_map)

  return(list(
    data = data,
    para = para,
    rand = rand,
    map = map
  ))
})


#' Update staRVe_model parameters / random effects from a fitted TMB::MakeADFun object
#'
#' @return A staRVe_model object with ML estimates
#'
#' @noRd
setMethod(f = "update_staRVe_model",
          signature = c(x = "staRVe_model",
                        y = "TMB_out"),
          definition = function(x,y) {
  sdr_est<- c(as.list(sdr(y),"Estimate",report=TRUE),
              as.list(sdr(y),"Estimate",report=FALSE))
  sdr_se<- c(as.list(sdr(y),"Std. Error",report=TRUE),
             as.list(sdr(y),"Std. Error",report=FALSE))
  par_names<- character(0)

  # Spatial parameters
  for( i in seq(.n_response(formula(x))) ) {
    spatial_parameters(x)[[i]]$par<- sdr_est$cv_pars[,i]
    spatial_parameters(x)[[i]]$se<- sdr_se$cv_pars[,i]
  }

  # Time parameters
  for( i in seq(.n_response(formula(x))) ) {
    time_parameters(x)[[i]]$par<- sdr_est$ts_pars[,i]
    time_parameters(x)[[i]]$se<- sdr_se$ts_pars[,i]
  }

  # Temporal random effects
  time_effects(x)$w<- sdr_est$ts_re
  time_effects(x)$se<- sdr_se$ts_re

  # Persistent graph random effects
  pg_re(x)$w<- sdr_est$pg_re
  pg_re(x)$se<- sdr_se$pg_re

  # Transient graph random effects
  if( nrow(locations(tg_re(x))) > 0 ) {
    values(tg_re(x))$w<- sdr_est$tg_re
    values(tg_re(x))$se<- sdr_se$tg_re
  } else {}

  # Fixed effects
  for( i in seq(.n_response(formula(x))) ) {
    fixed_effects(x)[[i]]$par<- sdr_est$beta[,i]
    fixed_effects(x)[[i]]$se<- sdr_se$beta[,i]
  }

  # Response distribution parameters
  for( i in seq(.n_response(formula(x))) ) {
    # Need seq(nrow(respone_parameters... to get rid of trailing NAs
    if( nrow(response_parameters(x)[[i]]) > 0 ) {
      response_parameters(x)[[i]]$par<- sdr_est$response_pars[seq(nrow(response_parameters(x)[[i]])),i]
      response_parameters(x)[[i]]$se<- sdr_se$response_pars[seq(nrow(response_parameters(x)[[i]])),i]
    } else {}
  }

  # Update the random effects for the observations
  s<- dat(x)$graph_idx
  t<- .time_from_formula(formula(x),dat(x))[[1]]-min(.time_from_formula(formula(x),dat(x)))+1
  std_tg_t<- .time_from_formula(formula(x),locations(tg_re(x))) - min(.time_from_formula(formula(x),dat(x)))+1
  for( i in seq(nrow(dat(x))) ) {
    if( s[[i]] <= dim(pg_re(x))[[1]] ) {
      values(data_predictions(x))$w[i,]<- pg_re(x)$w[s[[i]],t[[i]],]
      values(data_predictions(x))$w_se[i,]<- pg_re(x)$se[s[[i]],t[[i]],]
    } else {
      values(data_predictions(x))$w[i,]<- values(tg_re(x))$w[std_tg_t == t,][s[[i]]-dim(pg_re(x))[[1]],]
      values(data_predictions(x))$w_se[i,]<- values(tg_re(x))$se[std_tg_t == t,][s[[i]]-dim(pg_re(x))[[1]],]
    }
  }

  return(x)
})
