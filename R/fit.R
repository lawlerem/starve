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
  predictions<- dat(observations(fit))
  predictions<- .predict_linear(fit,predictions,predictions)
  predictions<- .predict_response(fit,predictions)

  dat(observations(fit))[,c("linear","linear_se","response","response_se")]<-
    predictions[,c("linear","linear_se","response","response_se"),drop=T]

  return(fit)
})

#' @param locations An sf object
#' @param covariates An sf object with covariates
#' @param time Which times to predict at?
#'
#' @export
#' @describeIn staRVe_model_fit Predict/forecast at specific locations
setMethod(f = "staRVe_predict",
          signature = c("staRVe_model_fit","sf"),
          definition = function(x,
                                locations,
                                covariates,
                                time = "model") {
  ### Check that we have all covariates, if there are covariates in the model
  covar_names<- .names_from_formula(formula(settings(x)))
  if( missing(covariates) && (length(covar_names) == 0) ) {
    covariates<- "missing"
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Please check covariate names.")
  } else {  }

  ### If time = "model", use timespan of original dataset
  model_times<- unique(random_effects(process(x))[,attr(random_effects(process(x)),"time_column"),drop=T])
  if( identical(time,"model") ) {
    time<- model_times
  } else {}

  # Get predictions and standard errors
  predictions<- .predict_w(x,
                           locations = locations,
                           pred_times = time)
  predictions<- .predict_linear(x,
                                predictions,
                                covariates)
  predictions<- .predict_response(x,
                                  predictions)

  # Re-order to w, w_se, linear, linear_se, response, response_se
  predictions[,1:6]<- predictions[,c(5,6,3,4,1,2),drop=T]
  colnames(predictions)[1:6]<- colnames(predictions)[c(5,6,3,4,1,2)]
  attr(predictions,"time_column")<- attr(random_effects(process(x)),"time_column")

  return(predictions)
})
#' @param ... Extra options
#'
#' @export
#' @describeIn staRVe_model_fit Predict/forecast over an entire raster
setMethod(f = "staRVe_predict",
          signature = c("staRVe_model_fit","RasterLayer"),
          definition = function(x,locations,covariates,time="model") {
  # Convert raster to sf
  prediction_points<- sf::st_as_sf(raster::rasterToPoints(locations,spatial=T))
  prediction_points<- prediction_points[,attr(prediction_points,"sf_column")]

  # Dispatch predictions based on if covariates are in the model and
  # if they are supplied
  covar_names<- .names_from_formula(formula(settings(x)))
  if( missing(covariates) && (length(covar_names) == 0) ) {
    pred<- staRVe_predict(x,prediction_points,time=time)
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Check the names of your raster covariates.")
  } else {
    covar_points<- .sf_from_raster_list(covariates,
      time_name=attr(random_effects(x),"time_column"))
    pred<- staRVe_predict(x,prediction_points,covar_points,time=time)
  }

  # Convert sf predictions to raster list
  pred_by_time<- split(pred,pred[,attr(random_effects(x),"time_column"),drop=T])
  pred_raster_by_time<- lapply(pred_by_time,raster::rasterize,locations)
  pred_raster_by_time<- lapply(pred_raster_by_time,function(raster_time) {
    ID_layer<- 1
    time_layer<- which(names(pred_raster_by_time[[1]]) == attr(random_effects(x),"time_column"))
    return(raster_time[[-c(ID_layer,time_layer)]])
  }) # Remove ID and time layer

  return(pred_raster_by_time)
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
    # If conditional, make sure the random effects are held constant
    TMB_input$data$conditional_sim<- conditional
    TMB_input$map$time_effects<- factor(rep(NA,length(TMB_input$para$time_effects)))
    TMB_input$map$resp_w<- factor(rep(NA,length(TMB_input$para$resp_w)))
    TMB_input$map$proc_w<- factor(rep(NA,length(TMB_input$para$proc_w)))
    TMB_input$map$pred_w<- factor(rep(NA,length(TMB_input$para$pred_w)))
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
  time_column<- attr(random_effects(model),"time_column")

  # Update random effects used for observations
  resp_w_idx<- 1
  for( i in seq(nrow(dat(model))) ) {
    if( length(edges(transient_graph(observations(model)))[[i]][[2]]) == 1 ) {
      # If length == 1, use random effects from persistent graph
      re<- random_effects(model)
      w<- re[,"w",drop=T][re[,time_column,drop=T] == dat(model)[i,time_column,drop=T]]
      dat(model)$w[[i]]<- w[[
                             edges(transient_graph(observations(model)))[[i]][[2]]
                           ]]
    } else {
      # If length > 1, use random effects from resp_w
      dat(model)$w[[i]]<- sims$resp_w[resp_w_idx]
      resp_w_idx<- resp_w_idx+1
    }
  }

  # Simulated values don't have standard errors, update linear and response predictions
  # to correspond to the simulated random effects
  dat(model)[,c("w_se","linear","linear_se","response","response_se")]<- NA
  dat(model)[,c("linear")]<- .predict_linear(model,dat(model),dat(model),se=F)[,"linear",drop=T]
  dat(model)[,c("response")]<- .predict_response(model,dat(model),se=F)[,c("response"),drop=T]

  # Update response observations to be the simulated value
  dat(model)[,attr(.response_from_formula(formula(settings(model)),
                                          dat(model)),"name")]<- sims$obs_y

  return(model)
})

#' Predict random effects from likelihood function
#'
#' @param x A starve_model_fit object
#' @param locations An sf object containing the prediction locations
#' @param pred_times Which times should predictions be made for?
#' @param dist_tol A small number so that prediction variances are not
#'   computationally singular
#'
#' @return An \code{sf} object with predictions for random effects (w) and
#'   their standard errors.
#'
#' @noRd
.predict_w<- function(x,
                      locations,
                      pred_times,
                      dist_tol = 0.00001,
                      ...) {
  # Get random effects, needed to compute prediction dag,
  random_effects<- random_effects(x)
  time_column<- attr(random_effects,"time_column")

  # Set up prediction data.frame
  # Each location is used each prediction time
  locations<- unique(locations[,attr(locations,"sf_column")])
  pred_times<- unique(pred_times)
  predictions<- do.call(rbind,lapply(pred_times,function(t) {
    df<- sf::st_sf(data.frame(w = 0,
                              w_se = NA,
                              time = t,
                              locations))
    colnames(df)[[3]]<- time_column
    return(df)
  }))
  attr(predictions,"time_column")<- time_column

  # Compute dag used for predictions
  dag<- construct_obs_dag(
    x = locations,
    y = split( # Random effect locations are duplicated, only want one representative
      random_effects,
      random_effects[,time_column,drop=T]
    )[[1]],
    time = 0, # Use the same graph every year
    check_intersection = T,
    settings = settings(x)
  )
  intersection_idx<- do.call(c,lapply(edges(dag),function(e) {
    return( length(e$from) )
  })) == 1
  # going to remove intersection locations, and adjust the remaining indices
  # adjust[i] is the number of intersection locations appearing before or at node i
  adjust<- numeric(nrow(locations))
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
  TMB_input$data$pred_w_time<- c(predictions[,time_column,drop=T]
    - min(random_effects(x)[,time_column,drop=T]))[!intersection_all_idx]
  TMB_input$data$pred_ws_edges<- edges(idxR_to_C(dag))[!intersection_idx]
  TMB_input$data$pred_ws_dists<- distances(dag)[!intersection_idx]

  TMB_input$para$pred_w<- predictions$w[!intersection_all_idx]
  TMB_input$map$pred_w<- NULL

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
  sdr<- summary(TMB::sdreport(obj,
                              par.fixed = opt(TMB_out(x))$par,
                              hessian.fixed = parameter_hessian(tracing(x)),
                              getReportCovariance = F))

  intersection_w_idx<- do.call(c,lapply(edges(dag)[intersection_idx],function(e) {
    return(e$from)
  }))

  intersection_w<- as.data.frame(sdr[rownames(sdr) == "proc_w",])
  intersection_w<- split(intersection_w,random_effects(x)[,time_column,drop=T])
  intersection_w<- lapply(intersection_w,`[`,i=intersection_w_idx,j=T)
  intersection_w<- intersection_w[names(intersection_w) %in% pred_times]
  intersection_w<- do.call(rbind,intersection_w)

  predictions[intersection_all_idx,c("w","w_se")]<- intersection_w
  predictions[!intersection_idx,c("w","w_se")] <- sdr[rownames(sdr) == "pred_w",]

  return(predictions)
}

#' Update random effect predictions to include covariates (link scale)
#'
#' @param x A staRVe_model object. If se = T, should be a staRVe_model_fit object.
#' @param w_predictions An sf object, typically the output of .predict_w. Must
#'   contain at least the columns w and w_se, and a time column if covariates are used.
#' @param covariates An sf object containing covariate information. Must have the same
#'   locations/times as w_predictions.
#' @param se Should standard errors be calculated?
#'
#' @return A data.frame including the supplied w_predictions and the predictions
#'   on the linear scale with standard errors
#'
#' @noRd
.predict_linear<- function(x,
                           w_predictions,
                           covariates,
                           se = T) {
  ### No intercept since it's already taken care of in predict_w
  if( identical(covariates,"missing") ) {
    # If there are no covariates, there's nothing to do
    linear<- w_predictions$w
    if( se ) { linear_se<- w_predictions$w_se } else { linear_se<- NA }
    return(sf::st_sf(data.frame(linear,
                                linear_se,
                                w_predictions)))
  } else {
    time_column<- attr(random_effects(x),"time_column")
    covar_names<- .names_from_formula(formula(settings(x)))

    # Split random effects by time so we can do a spatial join each year
    w_predictions<- w_predictions[,!(names(w_predictions) %in% covar_names)]
    w_predictions<- split(w_predictions,
                          w_predictions[,time_column,drop=T])

    # Split covariates by time, and take only the ones we need
    covariates<- covariates[,c(covar_names,time_column)]
    covariates<- split(covariates,covariates[,time_column,drop=T])
    covariates<- covariates[names(covariates) %in% names(w_predictions)]
    # names(covariates) and names(w_predictions) give the time

    # Do a spatial join of the predicted random effects and covariates,
    # and then stack them by time in an sf data.frame
    w_predictions<- do.call(rbind,lapply(names(w_predictions), function(t) {
      this_w<- w_predictions[[t]]
      this_covar<- covariates[[t]][,covar_names]
      suppressMessages(return(sf::st_join(this_w,this_covar,left=T)))
    }))

    # Create design matrix from covariates
    design<- .mean_design_from_formula(formula(settings(x)),
                                       w_predictions)

    # Create linear predictions
    beta<- fixed_effects(x)[,"par"]
    names(beta)<- rownames(fixed_effects(x))
    linear<- design %*% beta + w_predictions$w

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
      linear_se<- sqrt(rowSums((design %*% par_cov) * design) + w_predictions$w_se^2)
    } else {
      linear_se<- NA
    }

    return(sf::st_sf(data.frame(linear,
                                linear_se,
                                w_predictions)))
  }
}

#' Update predictions on link scale to the response scale
#'
#' A second-order Taylor approximation (delta method) is used
#'
#' @param x A staRVe_model object. If se = T, should be a staRVe_model_fit object.
#' @param linear_predictions A data.frame, typically the output of .predict_linear.
#'   Must contain at least the columns linear and linear_se.
#' @param se Should standard errors be calculated?
#'
#' @return A data.frame including the supplied linear_predictions and the predictions
#'   on the response scale with standard errors
#'
#' @noRd
.predict_response<- function(x,
                             linear_predictions,
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
    second_order_mean<- function(linear,linear_se) {
      mean<- link_function$fn(linear) + 0.5*link_function$he(linear)*linear_se^2
      return(as.numeric(mean))
    }
    response<- sapply(seq(nrow(linear_predictions)),function(i) {
      return(second_order_mean(linear_predictions[i,"linear",drop=T],
                               linear_predictions[i,"linear_se",drop=T]))
    })

    # Standard error =  f'(linear)^2*linear_se^2 + 0.5 * f''(linear)^2*linear_se^4
    second_order_se<- function(linear,linear_se) {
      se<- sqrt(link_function$gr(linear)^2*linear_se^2
        + 0.5*link_function$he(linear)^2*linear_se^4)
      return(as.numeric(se))
    }
    response_se<- sapply(seq(nrow(linear_predictions)),function(i) {
      return(second_order_se(linear_predictions[i,"linear",drop=T],
                             linear_predictions[i,"linear_se",drop=T]))
    })
  } else {
    # No standard error for second order delta method, just apply link function
    response<- sapply(linear_predictions$linear,link_function$fn)
    response_se<- NA
  }
  predictions<- sf::st_sf(data.frame(response,
                                     response_se,
                                     linear_predictions))
  return(predictions)
}
