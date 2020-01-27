#' @include classes.R access_staRVe.R access_TMB_out.R fit.R formula.R
NULL

.mc_integrate<- function(fn,
                         par_mean,
                         par_cov,
                         mc_samples = 500) {
  samples<- mvtnorm::rmvnorm(n = mc_samples,
                             mean = par_mean,
                             sigma = par_cov)
  evals<- apply(samples,MARGIN=1,fn)
  return(mean(evals))
}

.predict_w<- function(location,
                      x,
                      time,
                      n_neighbours,
                      max_dist,
                      distance_units,
                      parameter_covariance,
                      ...) {
  w<- process(x)
  w_times<- w[,settings(x)$time_column,drop=T]
  this_w<- w[w_times == time,]

  parents<- location$parents[[1]]

  this_time_w<- c(this_w[parents,"w",drop=T])
  w_se<- c(this_w[parents,"w_se",drop=T])
  if( length(w_se) > 0 ) {
    variance_inflation<- diag(c(0,w_se))
  } else {
    variance_inflation<- matrix(0,nrow=1,ncol=1)
  }

  if( (time-1) %in% names(w) ) {
    last_w<- w[w_times == (time-1),]
    last_time_w<- c(last_w[parents,"w",drop=T])
  } else{
    last_time_w<- rep(0,length(parents))
  }

  par_idx<- rownames(parameter_covariance) %in% c("logtau","logrho","logit_w_phi")
  par_cov<- parameter_covariance[par_idx,par_idx]

  data<- list(distances = location$dists[[1]],
              variance_inflation = variance_inflation,
              this_time_w = this_time_w,
              last_time_w = last_time_w)

  working_pars<- get_working_pars(x)[c("logtau","logrho","logit_w_phi"),]
  pars<- list(logtau = working_pars["logtau","par"],
              logrho = working_pars["logrho","par"],
              logit_w_phi = working_pars["logit_w_phi","par"])
  map<- obj(x)$env$map
  map<- map[names(map) %in% c("logtau","logrho","logit_w_phi")]

  data$return_type<- 0
  kriging_mean<- TMB::MakeADFun(data = data,
                                para = pars,
                                map = map,
                                DLL = "predict_w",
                                silent = TRUE)

  data$return_type<- 1
  kriging_variance<- TMB::MakeADFun(data = data,
                                    para = pars,
                                    map = map,
                                    DLL = "predict_w",
                                    silent = TRUE)
  pred_mean<- .mc_integrate(kriging_mean$fn,
                            kriging_mean$par,
                            par_cov,
                            ...)
  pred_var<- .mc_integrate(function(...) {
                             kriging_variance$fn(...) +
                             kriging_mean$fn(...)^2
                           },
                           kriging_variance$par,
                           par_cov,
                           ...)
  pred_var<- pred_var - pred_mean^2

  return(data.frame(w = pred_mean,w_se = sqrt(pred_var)))
}

.predict_linear<- function(x,
                           w,
                           w_se,
                           covariates,
                           parameter_covariance) {
  covar_names<- .names_from_formula(settings(x)$formula)
  if( missing(covariates) && (length(covar_names) == 0) ) {
    mean_design<- numeric(0)
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Please check covariate names.")
  } else {
    mean_design<- .mean_design_from_formula(settings(x)$formula,
                                            covariates)
    mean_design<- as.numeric(mean_design)
  }

  beta<- opt(x)$par[names(opt(x)$par) %in% c("mu","mean_pars")]

  par_idx<- rownames(parameter_covariance) %in% c("mu","mean_pars")
  par_cov<- parameter_covariance[par_idx,par_idx]

  mean_design<- c(1,mean_design)
  linear<- t(mean_design) %*% beta + w
  linear_se<- sqrt(t(mean_design) %*% par_cov %*% mean_design + w_se^2)

  return(data.frame(linear = linear,
                    linear_se = linear_se))
}

.predict_response<- function(x,
                             linear,
                             linear_se,
                             mc_samples = 100) {
  data<- list(link_code = settings(x)$link_code)
  pars<- list(x = 0)
  link_function<- TMB::MakeADFun(data = data,
                                 para = pars,
                                 DLL = "family",
                                 silent = TRUE)
  samples<- rnorm(mc_samples,linear,linear_se)
  samples<- sapply(samples,link_function$fn)

  return(data.frame(response = mean(samples),
                    response_se = sd(samples)))
}

#' Predict from a \code{staRVe} object at a single new location.
#'
#' @param location An object of class \code{sf} containing one point geometry.
#'   There should also be a \code{parents} column containing an integer vector
#'   entry, and a \code{dists} column containing a distance matrix.
#' @param x An object of class \code{staRVe}.
#' @param time The time at which to predict.
#' @param n_neighbours The maximum number of parents used to predict.
#'
#' @export
.predict_one_location<- function(location,
                                 x,
                                 time,
                                 covariates,
                                 n_neighbours,
                                 max_dist,
                                 distance_units,
                                 parameter_covariance,
                                 ...) {
  pred_w<- .predict_w(location = location,
                      x = x,
                      time = time,
                      n_neighbours = n_neighbours,
                      max_dist = max_dist,
                      distance_units = distance_units,
                      parameter_covariance = parameter_covariance,
                      ...)


  covar_names<- .names_from_formula(settings(x)$formula)
  if( missing(covariates) && (length(covar_names) == 0) ) {
    pred_linear<- .predict_linear(x,
                                  w = pred_w$w,
                                  w_se = pred_w$w_se,
                                  parameter_covariance = parameter_covariance)
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Check the names of your raster covariates.")
  } else {
    covariates<- covariates[covariates[,settings(x)$time_column,drop=T] == time,]
    suppressMessages(covariates<- sf::st_join(location,
                                              covariates,
                                              left = T))
    pred_linear<- .predict_linear(x,
                                  w = pred_w$w,
                                  w_se = pred_w$w_se,
                                  covariates = covariates,
                                  parameter_covariance = parameter_covariance)
  }


  pred_response<- .predict_response(x,
                                    linear = pred_linear$linear,
                                    linear_se = pred_linear$linear_se,
                                    ...)

  pred_location<- sf:::cbind.sf(pred_w,
                                pred_linear,
                                pred_response,
                                data.frame(time = time),
                                location[,attr(location,"st_geometry")])
  return(pred_location)
}

#' Reform a list of \code{Raster*} objects to an \code{sf} data.frame.
#'
#' @param x A list of \code{Raster*} objects.
#' @param time_name A string giving the name of the time variable.
#'
#' @return An \code{sf} data.frame.
.sf_from_raster_list<- function(x,time_name) {
  sf_list<- lapply(x,function(raster_brick) {
    layer_names<- names(raster_brick)
    layer_list<- lapply(layer_names,function(name) {
      numeric_name<- as.numeric(gsub("t","",name,ignore.case=T))
      sf_layer<- sf::st_as_sf(raster::rasterToPoints(raster_brick[[name]],spatial=T))
      sf_layer<- cbind(sf_layer[,name,drop=T],
                       numeric_name,
                       sf_layer[,attr(sf_layer,"st_geometry")])
      colnames(sf_layer)[1:2]<- c("value",time_name)
      return(sf_layer)
    })
    return(do.call(rbind,layer_list))
  })
  sf_list<- lapply(names(x), function(name) {
    colnames(sf_list[[name]])[[1]]<- name
    return(sf_list[[name]])
  })
  unique_times<- lapply(sf_list, function(x) {
    return(unique(x[,time_name,drop=T]))
  })
  unique_times<- sort(unique(do.call(c,unique_times)))

  unique_geoms<- unique(sf_list[[1]][,attr(sf_list[[1]],"st_geometry")])
  # This assumes the initial rasters are the same.


  sf_output<- lapply(unique_times,function(time) {
    year_sf<- cbind(time,unique_geoms)
    var_columns<- lapply(sf_list,function(var) {
      var_year<- var[var[,time_name,drop=T]==time,]
      var_year[,time_name]<- NULL
      suppressMessages(var_year<- sf::st_join(unique_geoms,var_year))
      var_year<- sf::st_drop_geometry(var_year)
      return(var_year)
    })
    var_df<- do.call(cbind.data.frame,var_columns)
    year_sf<- sf:::cbind.sf(var_df,year_sf)
    return(year_sf)
  })
  sf_output<- do.call(rbind,sf_output)
  colnames(sf_output)[colnames(sf_output)=="time"]<- time_name

  return(sf_output)
}

#' Predict from a \code{staRVe} object.
#'
#' @param x An object of class \code{staRVe}.
#' @param locations Either an object of class \code{sf} containing point geometries,
#'  or an object of class \code{RasterLayer}. If a \code{RasterLayer} object, predictions
#'  will be made for all raster cells whose value are not NA. If the raster has no values,
#'  then predictions will be made at every cell. This object should not have any
#'  time information.
#' @param covariates Either a data.frame of class \code{sf} or a list of \code{Raster*}
#'  objects, depending on the input type of \code{locations}.
#'
#'  If \code{locations} is of class \code{sf} with point geometries, then
#'  \code{covariates} should also be of class \code{sf}. The data.frame should contain
#'  columns for each of the covariates, a column for the time index, and a point
#'  geometry. For each time unit and prediction point, there should be a row
#'  in the \code{covariates} data.frame, but the rows do not need to be in the
#'  same order as \code{locations}.
#'
#'  If \code{locations} is of class \code{RasterLayer}, then \code{covariates}
#'  should be a list of \code{Raster*} objects. Each \code{Raster*} object should
#'  contain data for one covariate, should have one layer for each time unit,
#'  and should have the same raster geometry as the \code{locations} object. The
#'  layer names of each raster layer should be of the form \code{T####}, where
#'  \code{####} gives the specific time unit.
#' @param time What time units should predictions be made for? If set to "model",
#'  predictions are made for every time present in the model. The supplied time
#'  units must be a subset of those in the model.
#'
#' @return Either a \code{sf} object or a list of \code{Raster*} objects,
#'  containing predictions (with standard errors) for the spatial field and for
#'  the response. The return type is the same as the type of input for \code{locations}.
#'
#' @export
setGeneric(name = "predict_staRVe",
           def = function(x,locations,...) standardGeneric("predict_staRVe")

)
#' @param n_neighbours The maximum number of parents each node should have.
#'  If set to "model", then this number is taken from the model directed acyclic graph.
#' @param runOrderings A logical value. Should TMB::runSymbolicAnalysis be used?
#' @param get_sd Should standard errors be computed?
#'  Warning: computing standard errors can take a significant amount of time.
#' @param max_dist The maximum distance to search for parents. If set to "model,
#'  the maximum distance is set to the two times the spatial range parameter.
#'  Unless this has a units attribute, units are assumed to be the same as the model.
#'  See \code{\link{construct_dag}}.
#' @param parallel_cores How many cores to run in parallel? Defaults to one.
#'  See parallel::detectCores() for the number of cores on your machine. Uses
#'  parallel::mclapply, and thus never runs in parallel on Windows machines.
#'
#' @export
#' @rdname predict_staRVe
setMethod(f = "predict_staRVe",
          signature = c("staRVe","sf"),
          definition = function(x,
                                locations,
                                covariates,
                                n_neighbours = "model",
                                max_dist = "model",
                                time = "model",
                                ...) {
  covar_names<- .names_from_formula(settings(x)$formula)
  if( missing(covariates) && (length(covar_names) == 0) ) {
    covariates<- "missing"
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Please check covariate names.")
  } else {  }

  if( identical(n_neighbours,"model") ) {
    n_neighbours<- settings(x)$n_neighbours
  } else {}

  if( identical(max_dist,"model") ) {
    max_dist<- 2*parameters(x)[["rho","par"]]
  } else {}
  max_dist<- units::set_units(max_dist,settings(x)$distance_units,mode="standard")
  max_dist<- units::set_units(max_dist,"m")

  model_times<- unique(process(x)[,settings(x)$time_column,drop=T])
  if( identical(time,"model") ) {
    pred_times<- model_times
  } else {
    pred_times<- intersect(model_times,time)
  }

  fit_hessian<- numDeriv::hessian(obj(x)$fn,
                                    opt(x)$par)
  fit_cov<- solve(fit_hessian)
  rownames(fit_cov)<- colnames(fit_cov)<- names(opt(x)$par)

  locations<- locations[,attr(locations,"sf_column")]
  process_locs<- process(x)
  process_times<- process_locs[,settings(x)$time_column,drop=T]
  process_locs<- process_locs[process_times == pred_times[[1]],]
  dag<- construct_obs_dag(x = locations,
                          y = process_locs,
                          n_neighbours = n_neighbours,
                          check_intersection = F,
                          max_dist = max_dist,
                          distance_units = settings(x)$distance_units,
                          silent = T)
  locations$parents<- dag[[1]]
  locations$dists<- dag[[2]]

  location_list<- lapply(seq(nrow(locations)),function(i) {
    return(locations[i,]) # each row to a list element
  })

  pred_out_by_time<- parallel::mclapply(pred_times,
                                        mc.cores=1,
                                        FUN = function(time) {
    if( identical(covariates,"missing") ) {
      predictions<- lapply(location_list,
                           .predict_one_location,
                           x = x,
                           time = time,
                           n_neighbours = n_neighbours,
                           max_dist = as.numeric(max_dist),
                           distance_units = settings(x)$distance_units,
                           parameter_covariance = fit_cov,
                           ...)
    } else {
      predictions<- lapply(location_list,
                           .predict_one_location,
                           x = x,
                           time = time,
                           covariates = covariates,
                           n_neighbours = n_neighbours,
                           max_dist = max_dist,
                           distance_units = settings(x)$distance_units,
                           parameter_covariance = fit_cov,
                           ...)
    }
    predictions<- do.call(rbind,predictions)

    return(predictions)
  })

  prediction<- do.call(rbind,pred_out_by_time)
  rownames(prediction)<- NULL
  colnames(prediction)[colnames(prediction) == "time"]<- settings(x)$time_column

  return(prediction)
})

#' @export
#' @rdname predict_staRVe
setMethod(f = "predict_staRVe",
          signature = c("staRVe","RasterLayer"),
          definition = function(x,locations,covariates,...) {
    prediction_points<- sf::st_as_sf(raster::rasterToPoints(locations,spatial=T))
    prediction_points<- prediction_points[,attr(prediction_points,"sf_column")]

    covar_names<- .names_from_formula(settings(x)$formula)
    if( missing(covariates) && (length(covar_names) == 0) ) {
      pred<- predict_staRVe(x,prediction_points,...)
    } else if( missing(covariates) && (length(covar_names) != 0) ) {
      stop("Missing covariates, please supply them.")
    } else if( !all(covar_names %in% names(covariates)) ) {
      stop("Missing some covariates. Check the names of your raster covariates.")
    } else {
      covar_points<- .sf_from_raster_list(covariates,time_name=settings(x)$time_column)
      pred<- predict_staRVe(x,prediction_points,covar_points,...)
    }
    pred_by_time<- split(pred,pred[,settings(x)$time_column,drop=T])
    pred_raster_by_time<- lapply(pred_by_time,raster::rasterize,locations)
    pred_raster_by_time<- lapply(pred_raster_by_time,function(raster_time) {
      ID_layer<- 1
      time_layer<- raster::nlayers(raster_time)
      return(raster_time[[-c(ID_layer,time_layer)]])
    }) # Remove ID and time layer

    return(pred_raster_by_time)
})
