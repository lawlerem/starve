#' @include classes.R getset.R generics.R process.R observations.R parameters.R settings.R tracing.R TMB_out.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param process A process object
#' @param observations A observations object
#' @param tracing A tracing object
#' @param TMB_out A TMB_out object
#' @param settings A settings object
#'
#' @noRd
setMethod(
  f = "initialize",
  signature = "starve",
  definition = function(.Object,
                        process = new("process"),
                        observations = new("observations"),
                        tracing = new("tracing"),
                        TMB_out = new("TMB_out"),
                        settings = new("settings")) {
    process(.Object)<- process
    observations(.Object)<- observations
    tracing(.Object)<- tracing
    TMB_out(.Object)<- TMB_out
    settings(.Object)<- settings

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

# #' @param x An object
# #'
# #' @describeIn starve_class Get the process part of the model
#' @noRd
setMethod(f = "process",
          signature = "starve",
          definition = function(x) return(x@process)
)
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn starve_class Set the process part of the model
#' @noRd
setReplaceMethod(f = "process",
                 signature = "starve",
                 definition = function(x,value) {
  x@process<- value
  return(x)
})



# #' @param x An object
# #'
# #' @describeIn starve_class Get the observation part of the model
#' @noRd
setMethod(f = "observations",
          signature = "starve",
          definition = function(x) return(x@observations)
)
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn starve_class Set the observation part of the model
#' @noRd
setReplaceMethod(f = "observations",
                 signature = "starve",
                 definition = function(x,value) {
  x@observations<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn starve_class Get model settings
setMethod(f = "settings",
          signature = "starve",
          definition = function(x) return(x@settings)
)
# #' @param x An object
# #'
# #' @describeIn starve_class Set model settings
#' @noRd
setReplaceMethod(f = "settings",
                 signature = "starve",
                 definition = function(x,value) {
  x@settings<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn starve_class Get tracing information, see \link{tracing}.
setMethod(f = "tracing",
          signature = "starve",
          definition = function(x) return(x@tracing)
)
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn starve_class Set tracing information
#' @noRd
setReplaceMethod(f = "tracing",
                 signature = "starve",
                 definition = function(x,value) {
  x@tracing<- value
  return(x)
})


# #' @param x An object
# #'
# #' @describeIn starve_class Get TMB objects
#' @noRd
setMethod(f = "TMB_out",
          signature = "starve",
          definition = function(x) return(x@TMB_out)
)
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn starve_class Set TMB objects
#' @noRd
setReplaceMethod(f = "TMB_out",
                 signature = "starve",
                 definition = function(x,value) {
  x@TMB_out<- value
  return(x)
})



###################
### Meta-Access ###
###################


### From process

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get temporal random effects
setMethod(f = "time_effects",
          signature = "starve",
          definition = function(x) {
  return(time_effects(process(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set temporal random effects
setReplaceMethod(f = "time_effects",
                 signature = "starve",
                 definition = function(x,value) {
  time_effects(process(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get persistent graph random effects
setMethod(f = "pg_re",
          signature = "starve",
          definition = function(x) {
  return(pg_re(process(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set spatio-temporal random effects
setReplaceMethod(f = "pg_re",
                 signature = "starve",
                 definition = function(x,value) {
  pg_re(process(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get transient graph random effects
setMethod(f = "tg_re",
          signature = "starve",
          definition = function(x) {
  return(tg_re(process(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set transient graph random effects
setReplaceMethod(f = "tg_re",
                 signature = "starve",
                 definition = function(x,value) {
  tg_re(process(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get list containing the persistent graph random
#'   effects and the transient graph random effects.
setMethod(f = "random_effects",
          signature = "starve",
          definition = function(x) {
  return(random_effects(process(x)))
})




#' @param x An object
#'
#' @describeIn starve_class Get persistent graph
#' @export
setMethod(f = "persistent_graph",
          signature = "starve",
          definition = function(x) {
  return(persistent_graph(process(x)))
})
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn starve_class Set persistent graph
#' @noRd
setReplaceMethod(f = "persistent_graph",
                 signature = "starve",
                 definition = function(x,value) {
  persistent_graph(process(x))<- value
  return(x)
})


#' @param x An object
#'
#' @describeIn starve_class Get transient graph
#' @export
setMethod(f = "transient_graph",
          signature = "starve",
          definition = function(x) {
  return(transient_graph(process(x)))
})
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn starve_class Set transient graph
#' @noRd
setReplaceMethod(f = "transient_graph",
                 signature = "starve",
                 definition = function(x,value) {
  transient_graph(process(x))<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn starve_class Get a list containing the persistent and transient graphs
setMethod(f = "graph",
          signature = "starve",
          definition = function(x) {
  graph<- list(persistent_graph = persistent_graph(x),
               transient_graph = transient_graph(x))
  return(graph)
})



### From process_parameters

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get covariance function
setMethod(f = "covariance_function",
          signature = "starve",
          definition = function(x) {
  return(covariance_function(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set covariance function(s). Run
#'   get_starve_distributions("covariance") for valid covariance functions.
#'   Setting the covariance function also overwrites the spatial parameters.
setReplaceMethod(f = "covariance_function",
                 signature = "starve",
                 definition = function(x,value) {
  covariance_function(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get spatial parameters
setMethod(f = "space_parameters",
          signature = "starve",
          definition = function(x) {
  return(space_parameters(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set spatial parameters
setReplaceMethod(f = "space_parameters",
                 signature = "starve",
                 definition = function(x,value) {
  space_parameters(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get time parameters
setMethod(f = "time_parameters",
          signature = "starve",
          definition = function(x) {
  return(time_parameters(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set time parameters
setReplaceMethod(f = "time_parameters",
                 signature = "starve",
                 definition = function(x,value) {
  time_parameters(parameters(x))<- value
  return(x)
})



### From observations

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get data including response variables, time indices,
#'   locations, covariates, etc.
setMethod(f = "dat",
          signature = "starve",
          definition = function(x) {
  return(dat(observations(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set data. Warning: if you want to add new
#'   rows you must give each additional row a value in the graph_idx column,
#'   or create a new object from scratch.
setReplaceMethod(f = "dat",
                 signature = "starve",
                 definition = function(x,value) {
  dat(observations(x))<- value
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn starve_class Get data predictions, a long_stars object
#'   with the data (see \code{dat}) and associated random effect
#'   predictions.
setMethod(f = "data_predictions",
          signature = "starve",
          definition = function(x) {
  return(data_predictions(observations(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set data predictions
setReplaceMethod(f = "data_predictions",
                 signature = "starve",
                 definition = function(x,value) {
  data_predictions(observations(x))<- value
  return(x)
})




### From observation_parameters

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get the response distribution(s)
setMethod(f = "response_distribution",
          signature = "starve",
          definition = function(x) {
  return(response_distribution(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set the response distribution(s). Run
#'   get_starve_distributions("distribution") for valid options.
#'   Setting the response distribution also overwrites the response
#'   parameters and link function(s).
setReplaceMethod(f = "response_distribution",
                 signature = "starve",
                 definition = function(x,value) {
  response_distribution(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get response distribution parameters
setMethod(f = "response_parameters",
          signature = "starve",
          definition = function(x) {
  return(response_parameters(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set response distribution parameters
setReplaceMethod(f = "response_parameters",
                 signature = "starve",
                 definition = function(x,value) {
  response_parameters(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get link function(s)
setMethod(f = "link_function",
          signature = "starve",
          definition = function(x) {
  return(link_function(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set link function(s). Run
#'   get_starve_distributions("link") for valid link functions.
setReplaceMethod(f = "link_function",
                 signature = "starve",
                 definition = function(x,value) {
  link_function(parameters(x))<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get fixed effect parameters
setMethod(f = "fixed_effects",
          signature = "starve",
          definition = function(x) {
  return(fixed_effects(parameters(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set fixed effect parameters
setReplaceMethod(f = "fixed_effects",
                 signature = "starve",
                 definition = function(x,value) {
  fixed_effects(parameters(x))<- value
  return(x)
})


### From settings

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get distance units used for the model
setMethod(f = "distance_units",
          signature = "starve",
          definition = function(x) {
  return(distance_units(settings(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set distance units used for the model
setReplaceMethod(f = "distance_units",
                 signature = "starve",
                 definition = function(x,value) {
  ranges<- lapply(space_parameters(x),function(sp) {
    units::set_units(sp["range","par"],
                     distance_units(x),
                     mode="standard")
  })
  distance_units(settings(x))<- value
  distance_units(persistent_graph(process(x)))<- value
  distance_units(transient_graph(observations(x)))<- value
  ranges<- lapply(ranges,units::set_units,value,mode="standard")
  for( i in seq_along(ranges) ) {
    space_parameters(x)[[i]]["range","par"]<- units::drop_units(ranges[[i]])
  }
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn starve_class Get formula used for model
setMethod(f = "formula",
          signature = "starve",
          definition = function(x) {
  return(formula(settings(x)))
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set formula used for the model
setReplaceMethod(f = "formula",
                 signature = "starve",
                 definition = function(x,value) {
  if( !all(all.vars(value) %in% colnames(dat(x))) ) {
    stop("Not changing formula. Some variables present in new formula which are not available in dat(x)")
  } else {}
  formula(settings(x))<- value

  design<- mean_design_from_formula(value,dat(x))
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


# #' @param x An object
# #'
# #' @describeIn starve_class Get the name of the time variable used in
# #'   the model formula
#' @noRd
setMethod(f = "time_name",
          signature = "starve",
          definition = function(x) {
  return(time_name(settings(x)))
})


### Extras

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get all model parameters as a parameters object
setMethod(f = "parameters",
          signature = "starve",
          definition = function(x) {
  parameters<- new("parameters",
                   process_parameters = parameters(process(x)),
                   observation_parameters = parameters(observations(x)))
  return(parameters)
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn starve_class Set all model parameters with a new
#'   parameters object
setReplaceMethod(f = "parameters",
                 signature = c("starve","parameters"),
                 definition = function(x,value) {
  parameters(process(x))<- as(value,"process_parameters")
  parameters(observations(x))<- as(value,"observation_parameters")
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn starve_class Get convergence message from optimizer
setMethod(f = "convergence",
          signature = "starve",
          definition = function(x) {
  return(convergence(TMB_out(x)))
})

#' @param x An object
#'
#' @export
#' @describeIn starve_class Get all timing information, see \link{tracing}
setMethod(f = "timing",
          signature = "starve",
          definition = function(x) {
  return(timing(tracing(x)))
})




###############
###         ###
### Utility ###
###         ###
###############

# #' @describeIn TMB_in Convert a starve object to a list for \code{TMB::MakeADFun}
#' @noRd
setMethod(f = "TMB_in",
          signature = "starve",
          definition = function(x) {
  min_t<- min(stars::st_get_dimension_values(pg_re(x),time_name(x)))

  data<- list(
    model = "model",
    conditional_sim = FALSE,
    pg_edges = edges(idxR_to_C(persistent_graph(x))),
    pg_dists = distances(persistent_graph(x)),
    tg_t = c(time_from_formula(formula(settings(x)),locations(tg_re(x))))-min_t,
    tg_edges = edges(idxR_to_C(transient_graph(x))),
    tg_dists = distances(transient_graph(x)),
    cv_code = covariance_to_code(covariance_function(x)),
    distribution_code = distribution_to_code(response_distribution(x)),
    link_code = link_to_code(link_function(x)),
    obs = as.matrix(response_from_formula(formula(x),dat(x))),
    idx = cbind(
      dat(x)$graph_idx-1,
      c(time_from_formula(formula(x),dat(x)))-min_t
    ),
    sample_size = as.matrix(sample_size_from_formula(formula(x),dat(x),unique_vars=FALSE)),
    mean_design = as.matrix(mean_design_from_formula(formula(x),dat(x),"model.matrix")),
    pred_edges = vector(mode="list",length=0),
    pred_dists = vector(mode="list",length=0),
    pred_t = integer(0)
  )
  para<- list(
    ts_re = time_effects(x)[["w"]],
    working_ts_pars = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
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
        array(0,dim=c(0,n_response(formula(x))))
      } else {
        values(tg_re(x))[["w"]]
      }),
    working_cv_pars = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
        switch(covariance_function(x)[[v]],
          c(# Default -- all Matern-type covariance functions (exponential, gaussian, etc)
            ifelse( # std. dev. > 0
                space_parameters(x)[[v]]["sd","par"] > 0 ||
                space_parameters(x)[[v]]["sd","fixed"] == TRUE,
              log(space_parameters(x)[[v]]["sd","par"]),
              log(1)
            ),
            ifelse( # rho > 0
              space_parameters(x)[[v]]["range","par"] > 0 ||
              space_parameters(x)[[v]]["range","fixed"] == TRUE,
              log(space_parameters(x)[[v]]["range","par"]),
              log(100*mean(do.call(c,distances(graph(x)$persistent_graph))))
            ),
            ifelse( # nu > 0
                space_parameters(x)[[v]]["nu","par"] > 0 ||
                space_parameters(x)[[v]]["nu","fixed"] == TRUE,
              log(space_parameters(x)[[v]]["nu","par"]),
              log(0.5)
            )
          )
        )}
      )
    ),
    working_response_pars = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
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
              log(response_parameters(x)[[v]]["dispersion","par"]),
              log(1)
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
    beta = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
        fixed_effects(x)[[v]][colnames(data$mean_design),"par"]
      })
    ),
    pred_re = array(0,dim=c(0,n_response(formula(x))))
  )
  rand<- c("ts_re","pg_re","tg_re","pred_re")
  map<- list(
    working_ts_pars = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
        time_parameters(x)[[v]][,"fixed"]
      })
    ),
    working_cv_pars = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
        space_parameters(x)[[v]][,"fixed"]
      })
    ),
    working_response_pars = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
        switch((nrow(response_parameters(x)[[v]]) > 0)+1,
          logical(0),
          response_parameters(x)[[v]][,"fixed"]
        )
      })
    ),
    beta = do.call(cbind_no_recycle,
      lapply(seq(n_response(formula(x))),function(v) {
        fixed_effects(x)[[v]][colnames(data$mean_design),"fixed"]
      })
    )
  )
  map<- lapply(map,logical_to_map)

  return(list(
    data = data,
    para = para,
    rand = rand,
    map = map
  ))
})


# #' @describeIn strv_update Update model parameters and random effects from a
# #'   fitted TMB::MakeADFun object
#' @noRd
setMethod(f = "strv_update",
          signature = c(x = "starve",
                        y = "TMB_out"),
          definition = function(x,y) {
  sdr_est<- c(as.list(sdr(y),"Estimate",report=TRUE),
              as.list(sdr(y),"Estimate",report=FALSE))
  sdr_se<- c(as.list(sdr(y),"Std. Error",report=TRUE),
             as.list(sdr(y),"Std. Error",report=FALSE))
  par_names<- character(0)

  # Spatial parameters
  for( i in seq(n_response(formula(x))) ) {
    space_parameters(x)[[i]]$par<- sdr_est$cv_pars[,i]
    space_parameters(x)[[i]]$se<- sdr_se$cv_pars[,i]
  }

  # Time parameters
  for( i in seq(n_response(formula(x))) ) {
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
  for( i in seq(n_response(formula(x))) ) {
    fixed_effects(x)[[i]]$par<- sdr_est$beta[,i]
    fixed_effects(x)[[i]]$se<- sdr_se$beta[,i]
  }

  # Response distribution parameters
  for( i in seq(n_response(formula(x))) ) {
    # Need seq(nrow(respone_parameters... to get rid of trailing NAs
    if( nrow(response_parameters(x)[[i]]) > 0 ) {
      response_parameters(x)[[i]]$par<- sdr_est$response_pars[seq(nrow(response_parameters(x)[[i]])),i]
      response_parameters(x)[[i]]$se<- sdr_se$response_pars[seq(nrow(response_parameters(x)[[i]])),i]
    } else {}
  }

  # Update the random effects for the observations
  s<- dat(x)$graph_idx
  t<- time_from_formula(formula(x),dat(x))-min(stars::st_get_dimension_values(pg_re(x),time_name(x)))+1
  std_tg_t<- time_from_formula(formula(x),locations(tg_re(x))) - min(stars::st_get_dimension_values(pg_re(x),time_name(x)))+1
  for( i in seq(nrow(dat(x))) ) {
    if( s[[i]] <= dim(pg_re(x))[[1]] ) {
      values(data_predictions(x))$w[i,]<- pg_re(x)$w[s[[i]],t[[i]],]
      values(data_predictions(x))$w_se[i,]<- pg_re(x)$se[s[[i]],t[[i]],]
    } else {
      values(data_predictions(x))$w[i,]<- values(tg_re(x))$w[std_tg_t == t[[i]],,drop=FALSE][s[[i]]-dim(pg_re(x))[[1]],]
      values(data_predictions(x))$w_se[i,]<- values(tg_re(x))$se[std_tg_t == t[[i]],,drop=FALSE][s[[i]]-dim(pg_re(x))[[1]],]
    }
  }

  return(x)
})
