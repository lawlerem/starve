#' @param x An object
#' @param silent Should intermediate calculations be printed?
#' @param ... Extra options to supply to TMB::MakeADFun or TMB::sdreport
#'
#' @export
#' @describeIn staRVe_model Find maximum likelihood estimate of parameters
#'   and random effects
setMethod(f = "staRVe_fit",
          signature = "staRVe_model",
          definition = function(x,silent = F,...) {
  TMB_input<- TMB_in(x)

  TMB_out<- new("TMB_out")
  tracing<- new("staRVe_tracing")

  obj(TMB_out)<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "staRVe_model",
    silent = silent,
    ...
  )

  system.time({
    if( length(obj(TMB_out)$par) > 0 ) {
      # If there are parameters, find ML estimates
      opt(TMB_out)<- nlminb(obj(TMB_out)$par,
                            obj(TMB_out)$fn,
                            obj(TMB_out)$gr)
    } else {
      # If there are no parameters, create an empty nlminb output
      opt(TMB_out)<- list(
        par = numeric(0),
        objective = obj(TMB_out)$fn(),
        convergence = 0,
        iterations = 0,
        evaluations = c("function"=0,gradient=0),
        message = "No parameters to optimize (NA)"
      )
    }
  }) -> opt_time(tracing)

  system.time({
    if( length(opt(TMB_out)$par) > 0 ) {
      # Compute hessian and covariance matrix
      hess<- optimHess(opt(TMB_out)$par,
                       obj(TMB_out)$fn,
                       obj(TMB_out)$gr)
      par_cov<- solve(hess)

      rownames(hess)<-
        colnames(hess)<-
        rownames(par_cov)<-
        colnames(par_cov)<-
        names(opt(TMB_out)$par)
    } else {
      hess<- par_cov<- matrix(0,nrow=0,ncol=0)
    }

    parameter_hessian(tracing)<- hess
    parameter_covariance(tracing)<- par_cov
  }) -> hess_time(tracing)

  system.time({
    # Get standard errors for parameters and random effects
    sdr(TMB_out)<- TMB::sdreport(
      obj(TMB_out),
      par.fixed = opt(TMB_out)$par,
      hessian.fixed = parameter_hessian(tracing),
      getReportCovariance = F,
      ...
    )
  }) -> sdr_time(tracing)

  # Update staRVe_model based on parameter estimates
  fit<- new("staRVe_model_fit",
            staRVe_model = x,
            tracing = tracing,
            TMB_out = TMB_out)
  as(fit,"staRVe_model")<- update_staRVe_model(
    x = as(fit,"staRVe_model"),
    y = TMB_out(fit)
  )

  # Get random effects, predictions on link scale, and predictions on response scale
  data_predictions(fit)<- .predict_linear(fit,data_predictions(fit))
  data_predictions(fit)<- .predict_response(fit,data_predictions(fit))

  # dat(fit)[,c("linear","linear_se")]<- .predict_linear(fit,unique(w_predictions),dat(fit))[,c("linear","linear_se"),drop=T]
  # dat(fit)[,c("response","response_se")]<- .predict_response(fit,dat(fit))[,c("response","response_se"),drop=T]

  return(fit)
})

#' @param locations An sf object for prediction locations and times, with covariates
#'
#' @export
#' @describeIn staRVe_model_fit Predict/forecast at specific locations
setMethod(f = "staRVe_predict",
          signature = c("staRVe_model_fit","sf"),
          definition = function(x,
                                locations) {
  ### Check that we have all covariates, if there are covariates in the model
  covar_names<- .names_from_formula(formula(x))

  if( !all(covar_names %in% colnames(locations)) ) {
    stop("Missing covariates, please add them to the prediction locations.")
  } else {}
  predictions<- new("staRVe_predictions",locations)

  # Get predictions and standard errors
  predictions<- .predict_w(x,predictions)
  predictions<- .predict_linear(x,predictions)
  predictions<- .predict_response(x,predictions)

  return(predictions)
})
#' @param covariates A list of \code{Raster*} objects for raster predictions.
#'  If the model has no covariates, then nothing needs to be supplied.
#'
#'  If \code{locations} is of class \code{RasterLayer}, then \code{covariates}
#'  should be a list of \code{Raster*} objects. Each \code{Raster*} object should
#'  contain data for one covariate, should have one layer for each time unit,
#'  and should have the same raster geometry as the \code{locations} object. The
#'  layer names of each raster layer should be of the form \code{T####}, where
#'  \code{####} gives the specific time index. The geometry of all the
#'  \code{Raster*} objects should be identical.
#'
#'  #'  If \code{locations} is of class \code{sf} with point geometries, then
#'  covariate values should be included in that \code{sf} object.
#' @param time What time indices should predictions be made for raster prediction?
#'  If set to "model", predictions are made for every time present in the model.
#'
#' @export
#' @describeIn staRVe_model_fit Predict/forecast over an entire raster
setMethod(f = "staRVe_predict",
          signature = c("staRVe_model_fit","RasterLayer"),
          definition = function(x,locations,covariates,time="model") {
  # Convert raster to sf
  uniq_prediction_points<- sf::st_as_sf(raster::rasterToPoints(locations,spatial=T))
  uniq_prediction_points<- uniq_prediction_points[,attr(uniq_prediction_points,"sf_column")]
  if( identical(time,"model") ) {
    time<- seq(min(dat(x)[,.time_name(x),drop=T]),max(dat(x)[,.time_name(x),drop=T]))
  } else {
    time<- seq(min(time),max(time))
  }
  prediction_points<- sf::st_sf(data.frame(time = rep(time,each=nrow(uniq_prediction_points)),
                                           geom = rep(sf::st_geometry(uniq_prediction_points),length(time))))
  colnames(prediction_points)[[1]]<- .time_name(x)

  # Dispatch predictions based on if covariates are in the model and
  # if they are supplied
  covar_names<- .names_from_formula(formula(settings(x)))
  if( missing(covariates) && (length(covar_names) == 0) ) {
    pred<- staRVe_predict(x,prediction_points)
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Check the names of your raster covariates.")
  } else {
    covar_points<- .sf_from_raster_list(covariates,time_name=.time_name(x))
    colnames(prediction_points)[[2]]<- attr(covar_points,"sf_column")
    sf::st_geometry(prediction_points)<- attr(covar_points,"sf_column")
    prediction_points<- do.call(rbind,Map(
      sf::st_join,
      split(prediction_points,prediction_points[,.time_name(x),drop=T]),
      split(covar_points,covar_points[,.time_name(x),drop=T]),
      suffix = lapply(unique(covar_points[,.time_name(x),drop=T]),function(t) return(c("",".a")))
    ))

    prediction_points[,paste0(.time_name(x),".a")]<- NULL
    pred<- staRVe_predict(x,prediction_points)
  }

  # Convert predictions to stars object
  # should have dimension 3 : x coordinate, y coordinate, time
  pred_sf_list<- cbind(do.call(cbind,predictions(pred)),locations(pred))
  pred_sf_list<- lapply(split(pred_sf_list,pred_sf_list[,.time_name(x),drop=T]),
                        stars::st_rasterize,
                        template = stars::st_as_stars(locations))
  pred_stars<- do.call(c,c(pred_sf_list,list(along = .time_name(x))))
  attr(pred_stars,"dimensions")[[.time_name(x)]]$delta<- 1
  attr(pred_stars,"dimensions")[[.time_name(x)]]$offset<- min(time)
  pred_stars[[.time_name(x)]]<- NULL

  return(pred_stars)
})



#' @param model A staRVe_model
#' @param conditional logical. If true, new observations are simulated conditional
#'   on the random effect values in \code{random_effects(model)}.
#'   If false, new random effects and new observations are simulated.
#' @export
#' @describeIn staRVe_model Simulate from the model
setMethod(f = "staRVe_simulate",
          signature = "staRVe_model",
          def = function(model,
                         conditional = F,
                         ...) {
  TMB_input<- TMB_in(model)
  if( conditional ) {
    # If conditional, the random effects are not re-simulated
    TMB_input$data$conditional_sim<- conditional
  } else {}

  obj<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "staRVe_model",
    silent = T,
    ...
  )

  # Parameters are simulated from, not estimated, so get rid of standard errors
  spatial_parameters(model)$se<- NA
  time_parameters(model)$se<- NA
  response_parameters(model)$se<- rep(NA,nrow(response_parameters(model)))
  fixed_effects(model)$se<- rep(NA,nrow(fixed_effects(model)))

  # Simulated random effects
  sims<- obj$simulate()
  time_effects(model)$w<- sims$time_effects
  time_effects(model)$se<- NA
  random_effects(model)$w<- sims$proc_w
  random_effects(model)$se<- NA

  # Update random effects used for observations
  resp_w_idx<- 1
  for( i in seq(nrow(dat(model))) ) {
    if( length(edges(graph(model)$transient_graph)[[i]][["from"]]) == 1 ) {
      # If length == 1, take random effects from persistent graph
      this_year<- dat(model)[i,.time_name(model),drop=T]
      predictions(data_predictions(model))$w[[i]]<- as.numeric(
        random_effects(model)["w",
                              edges(graph(model)$transient_graph)[[i]][["from"]],
                              which(stars::st_get_dimension_values(random_effects(model),.time_name(model))==this_year),
                              drop=T]
      )
    } else {
      # If length > 1, use random effects from resp_w
      predictions(data_predictions(model))$w[[i]]<- sims$resp_w[resp_w_idx]
      resp_w_idx<- resp_w_idx+1
    }
  }
  predictions(data_predictions(model))$w_se[]<- NA

  # Simulated values don't have standard errors, update linear and response predictions
  # to correspond to the simulated random effects
  data_predictions(model)<- .predict_linear(model,data_predictions(model),se = F)
  data_predictions(model)<- .predict_response(model,data_predictions(model),se=F)

  # Update response observations to be the simulated value
  dat(model)[,attr(.response_from_formula(formula(settings(model)),
                                          dat(model)),"name")]<- sims$obs_y

  return(model)
})

#' Predict random effects from likelihood function
#'
#' @param x A starve_model_fit object
#' @param predictions A staRVe_predictions object
#' @param dist_tol A small number so that prediction variances are not
#'   computationally singular
#'
#' @return An \code{sf} object with predictions for random effects (w) and
#'   their standard errors.
#'
#' @noRd
.predict_w<- function(x,
                      predictions,
                      dist_tol = 0.00001,
                      ...) {
  # Get random effects, needed to compute prediction dag,
  random_effects<- random_effects(x)

  # Set up prediction data.frame
  # Each location is used each prediction time
  locs<- unique(locations(predictions)[,attr(locations(predictions),"sf_column")])
  pred_times<- unique(locations(predictions)[,.time_name(x),drop=T])
  full_predictions<- new("staRVe_predictions",
                         sf::st_sf(data.frame(time = rep(pred_times,each=nrow(locs)),
                                              geom = rep(sf::st_geometry(locs),length(pred_times))))
                        )
  colnames(locations(full_predictions))[[1]]<- .time_name(x)

  # Compute dag used for predictions
  dag<- construct_obs_dag(
    x = locs,
    y = .locations_from_stars(random_effects(x)),
    time = 0, # Use the same graph every year
    check_intersection = T,
    settings = settings(x)
  )
  # intersection_idx is the same every year
  intersection_idx<- do.call(c,lapply(edges(dag),function(e) {
    return( length(e$from) )
  })) == 1
  # going to remove intersection locations, and adjust the remaining indices
  # adjust[i] is the number of intersection locations appearing before or at node i
  adjust<- numeric(nrow(locs))
  lapply(edges(dag)[intersection_idx],function(e) {
    idx<- seq_along(adjust) >= e$to
    adjust[idx]<<- adjust[idx]+1
  })
  edges(dag)[!intersection_idx]<- lapply(edges(dag)[!intersection_idx],function(e) {
    e$to<- e$to-adjust[e$to]
    return(e)
  })


  intersection_all_idx<- rep(intersection_idx,length(pred_times))

  # Add random effects for times not present in the original model,
  # only added to pass to TMB
  x<- .add_random_effects_by_time(x,pred_times)

  # Prepare input for TMB, TMB_in(x) doesn't take care of pred_* things
  # except pred_w is already declared as a random effect
  TMB_input<- TMB_in(x)
  TMB_input$data$pred_w_time<- c(locations(full_predictions)[,.time_name(x),drop=T]
    - min(stars::st_get_dimension_values(random_effects(x),.time_name(x))))[!intersection_all_idx]
  TMB_input$data$pred_ws_edges<- edges(idxR_to_C(dag))[!intersection_idx]
  TMB_input$data$pred_ws_dists<- distances(dag)[!intersection_idx]

  TMB_input$para$pred_w<- predictions(full_predictions)$w[!intersection_all_idx]

  # Create the TMB object and evaluate it at the ML estimates
  obj<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "staRVe_model",
    silent = TRUE,
    ...
  )
  obj$fn(opt(TMB_out(x))$par)

  # Get standard errors at ML estimates
  sdr<- TMB::sdreport(obj,
                      par.fixed = opt(TMB_out(x))$par,
                      hessian.fixed = parameter_hessian(tracing(x)),
                      getReportCovariance = F)

  # intersection_w_idx is the same every year
  intersection_w_idx<- do.call(c,lapply(edges(dag)[intersection_idx],function(e) {
    return(e$from)
  }))

  if( any(intersection_idx) ) {
    intersection_w<- as.list(sdr,"Estimate")$proc_w
    intersection_w<- apply(intersection_w,MARGIN=2,`[`,intersection_w_idx) # MARGIN=2 is time
    intersection_w<- intersection_w[,stars::st_get_dimension_values(random_effects(x),.time_name(x)) %in% pred_times]
    intersection_w<- c(intersection_w)

    intersection_se<- as.list(sdr,"Std. Error")$proc_w
    intersection_se<- apply(intersection_se,MARGIN=2,`[`,intersection_w_idx) # MARGIN=2 is time
    intersection_se<- intersection_se[,stars::st_get_dimension_values(random_effects(x),.time_name(x)) %in% pred_times]
    intersection_se<- c(intersection_se)

    predictions(full_predictions)$w[intersection_all_idx]<- intersection_w
    predictions(full_predictions)$w_se[intersection_all_idx]<- intersection_se
  } else {}
  predictions(full_predictions)$w[!intersection_idx]<- as.list(sdr,"Estimate")$pred_w
  predictions(full_predictions)$w_se[!intersection_idx]<- as.list(sdr,"Std. Error")$pred_w
  # predictions[!intersection_idx,c("w","w_se")] <- summary(sdr)[rownames(summary(sdr)) == "pred_w",]


  # Pick out year / location combos that were in original
  idx<- lapply(Map(
    st_intersects,
    split(locations(predictions),locations(predictions)[,.time_name(x),drop=T]),
    split(locations(full_predictions),locations(full_predictions)[,.time_name(x),drop=T]),
    sparse = T
  ),as.data.frame)
  idx<- do.call(c,lapply(seq_along(idx),function(t) {
    id<- idx[[t]]$col.id + (t-1)*nrow(locs) # locs is unique(locations)
    return(id)
  }))
  predictions(predictions)$w<- predictions(full_predictions)$w[idx]
  predictions(predictions)$w_se<- predictions(full_predictions)$w_se[idx]

  return(predictions)
}

#' Update random effect predictions to include covariates (link scale)
#'
#' @param x A staRVe_model object. If se = T, should be a staRVe_model_fit object.
#' @param predictions A staRVe_predictions object, typically the output of .predict_w.
#'   Should contain covariate data in slot 'locations'
#' @param se Should standard errors be calculated?
#'
#' @return A staRVe_predictions object with predictions and standard errors for the linear term.
#'
#' @noRd
.predict_linear<- function(x,
                           predictions,
                           se = T) {
  ### No intercept since it's already taken care of in predict_w
  if( length(.names_from_formula(formula(x))) == 0 ) {
    # If there are no covariates, there's nothing to do
    predictions(predictions)$linear<- predictions(predictions)$w
    if( se ) { predictions(predictions)$linear_se<- predictions(predictions)$w_se }
      else { predictions(predictions)$linear_se<- NA }
  } else {
    # Create design matrix from covariates
    design<- as.matrix(.mean_design_from_formula(formula(x),
                                                 locations(predictions)))

    # Create linear predictions
    beta<- fixed_effects(x)[,"par"]
    names(beta)<- rownames(fixed_effects(x))
    predictions(predictions)$linear<- design %*% beta + cbind(predictions(predictions)$w)

    if( se ) {
      # Create parameter covariance estimate for fixed effects
      par_cov<- matrix(0,ncol=length(beta),nrow=length(beta))
      colnames(par_cov)<- rownames(par_cov)<- row.names(fixed_effects(x))

      # Fill in the covariance matrix with the standard errors for fixed effect
      # coefficients.
      parameter_covariance<- parameter_covariance(tracing(x))
      par_idx<- rownames(parameter_covariance) %in% c("mean_pars")
      par_sdreport<- parameter_covariance[par_idx,par_idx,drop=F] # Drop = F to keep matrix
      # Keep fixed fixed effects with an standard error of 0
      par_idx<- names(beta)[fixed_effects(parameters(x))[,"fixed"] == F]
      par_cov[par_idx,par_idx]<- par_sdreport

      # The commented and uncommented methods are the same
      # linear_se<- sqrt(diag(design %*% par_cov %*% t(design)) + w_predictions$w_se^2)
      predictions(predictions)$linear_se<- sqrt(rowSums((design %*% par_cov) * design) + predictions(predictions)$w_se^2)
    } else {
      predictions(predictions)$linear_se<- NA
    }
  }
  return(predictions)
}

#' Update predictions on link scale to the response scale
#'
#' A second-order Taylor approximation (delta method) is used
#'
#' @param x A staRVe_model object. If se = T, should be a staRVe_model_fit object.
#' @param predictions A data.frame, typically the output of .predict_linear.
#'   Must contain at least the columns linear and linear_se.
#' @param se Should standard errors be calculated?
#'
#' @return A data.frame including the supplied linear_predictions and the predictions
#'   on the response scale with standard errors
#'
#' @noRd
.predict_response<- function(x,
                             predictions,
                             se = T) {
  # Just need to specify which link function is used
  # Will get the function and gradient from TMB, evaluated at
  # linear and linear_se
  data<- list(
    model = "family",
    link_code = .link_to_code(link_function(x))
  )
  para<- list(x = 0)
  link_function<- TMB::MakeADFun(
    data = data,
    para = para,
    DLL = "staRVe_model",
    silent = T
  )

  if( se ) {
    # Mean prediction = f(linear) + 0.5 * f''(linear) * linear_se^2
    second_order_mean<- Vectorize(function(linear,linear_se) {
      mean<- link_function$fn(linear) + 0.5*link_function$he(linear)*linear_se^2
      return(as.numeric(mean))
    },SIMPLIFY = "array")
    predictions(predictions)$response<- second_order_mean(predictions(predictions)$linear,
                                                          predictions(predictions)$linear_se)

    # Standard error =  f'(linear)^2*linear_se^2 + 0.5 * f''(linear)^2*linear_se^4
    second_order_se<- Vectorize(function(linear,linear_se) {
      se<- sqrt(link_function$gr(linear)^2*linear_se^2
        + 0.5*link_function$he(linear)^2*linear_se^4)
      return(as.numeric(se))
    },SIMPLIFY = "array")
    predictions(predictions)$response_se<- second_order_se(predictions(predictions)$linear,
                                                           predictions(predictions)$linear_se)
  } else {
    # No standard error for second order delta method, just apply link function
    predictions(predictions)$response<- Vectorize(link_function$fn,SIMPLIFY = "array")(predictions(predictions)$linear)
    predictions(predictions)$response_se<- NA
  }

  return(predictions)
}
