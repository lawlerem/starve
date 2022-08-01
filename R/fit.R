#' @param silent Should intermediate calculations be printed?
#'
#' @describeIn strv_fit Takes an unfitted starve object and performs
#'   maximum likelihood inference via the \code{TMB} package to obtain parameter
#'   and random effect estimates with their associated standard errors. Options
#'   in the \dots argument are passed to TMB::MakeADFun and TMB::sdreport.
#'
#' @export
setMethod(f = "strv_fit",
          signature = "starve",
          definition = function(object,silent = FALSE,...) {
  TMB_input<- TMB_in(object)

  TMB_out<- new("TMB_out")
  tracing<- new("tracing")

  obj(TMB_out)<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "starve_TMB",
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
      getReportCovariance = FALSE,
      ...
    )
  }) -> sdr_time(tracing)

  # Update starve object based on parameter estimates
  tracing(object)<- tracing
  TMB_out(object)<- TMB_out
  object<- strv_update(object,TMB_out)

  # Get random effects, predictions on link scale, and predictions on response scale
  data_predictions(object)<- predict_linear(object,data_predictions(object))
  data_predictions(object)<- predict_response(object,data_predictions(object))

  return(object)
})


#' @param conditional Logical. If false (default), new random effects and new
#'   observations are simulated. If true, new observations are simulated
#'   conditional on the random effect values in the supplied model.
#
#' @export
#' @describeIn strv_simulate Simulate a new dataset from the model, with the
#' option to simulate a new set of random effects as well. The parameter values
#' used in the simulations are those set in \code{parameters(model)}. Returns
#' a \code{starve} object with simulated random effects (if \code{conditional=FALSE})
#' and simulated data.
setMethod(f = "strv_simulate",
          signature = "starve",
          def = function(object,
                         conditional = FALSE,
                         ...) {

  TMB_input<- TMB_in(object)
  if( conditional ) {
    # If conditional, the random effects are not re-simulated
    TMB_input$data$conditional_sim<- conditional
  } else {}

  obj<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "starve_TMB",
    silent = TRUE,
    ...
  )

  # Parameters are simulated from, not estimated, so get rid of standard errors
  for( i in seq_along(response_names(formula(object))) ) {
    spatial_parameters(object)[[i]]$se<- NA
    time_parameters(object)[[i]]$se<- NA
    response_parameters(object)[[i]]$se<- rep(NA,nrow(response_parameters(object)[[i]]))
    fixed_effects(object)[[i]]$se<- rep(NA,nrow(fixed_effects(object)[[i]]))
  }

  # Simulated random effects
  sims<- obj$simulate()
  time_effects(object)$w<- sims$ts_re
  time_effects(object)$se<- NA
  pg_re(object)$w<- sims$pg_re
  pg_re(object)$se<- NA
  if( nrow(locations(tg_re(object))) > 0 ) {
    values(tg_re(object))$w<- sims$tg_re
  } else {}
  values(tg_re(object))$se<- NA

  # Update random effects used for observations
  s<- dat(object)$graph_idx
  t<- time_from_formula(formula(object),dat(object))-min(time_from_formula(formula(object),dat(object)))+1
  std_tg_t<- time_from_formula(formula(object),locations(tg_re(object))) - min(time_from_formula(formula(object),dat(object)))+1
  for( i in seq(nrow(dat(object))) ) {
    if( s[[i]] <= dim(pg_re(object))[[1]] ) {
      values(data_predictions(object))$w[i,]<- pg_re(object)$w[s[[i]],t[[i]],]
    } else {
      values(data_predictions(object))$w[i,]<- values(tg_re(object))$w[std_tg_t == t[[i]],,drop=FALSE][s[[i]]-dim(pg_re(object))[[1]],]
    }
  }
  values(data_predictions(object))$w_se[]<- NA

  # Simulated values don't have standard errors, update linear and response predictions
  # to correspond to the simulated random effects
  data_predictions(object)<- predict_linear(object,data_predictions(object),se = FALSE)
  data_predictions(object)<- predict_response(object,data_predictions(object),se=FALSE)

  # Update response observations to be the simulated value
  dat(object)[,attr(response_from_formula(formula(settings(object)),
                                          dat(object)),"name")]<- sims$obs

  # Update convergence/message and TMB::MakeADFun obj
  opt(TMB_out(object))$convergence<- 0
  opt(TMB_out(object))$message<- "Simulated realization from model"
  obj(TMB_out(object))<- obj

  return(object)
})


#' @export
#' @describeIn strv_predict Predict/forecast at the specific locations and
#'   times given in \code{new_data}. Any covariates used to fit the model
#'   should be included in the rows of \code{new_data}. Returns a \code{long_stars}
#'   object containing a copy of \code{new_data} and the associated predictions
#'   and standard errors for the random effects and response mean on both the
#'   link and natural scale.
setMethod(f = "strv_predict",
          signature = c("starve","sf"),
          definition = function(x,
                                new_data) {
  ### Check that we have all covariates, if there are covariates in the model
  covar_names<- names_from_formula(formula(x))

  if( !all(covar_names %in% colnames(new_data)) ) {
    stop("Missing covariates, please add them to the prediction locations.")
  } else {}
  predictions<- new("long_stars",new_data,var_names=response_names(formula(x)))

  # Get predictions and standard errors
  predictions<- predict_w(x,predictions)
  predictions<- predict_linear(x,predictions)
  predictions<- predict_response(x,predictions)

  return(predictions)
})


#' @param covariates A list of \code{Raster*} objects for raster predictions.
#'  If the model has no covariates, then nothing needs to be supplied.
#'
#'  If \code{new_data} is of class \code{RasterLayer}, then \code{covariates}
#'  should be a list of \code{Raster*} objects. Each \code{Raster*} object should
#'  contain data for one covariate, should have one layer for each time unit,
#'  and should have the same raster geometry as the \code{new_data} object. The
#'  layer names of each raster layer should be of the form \code{T####}, where
#'  \code{####} gives the specific time index. The geometry of all the
#'  \code{Raster*} objects should be identical.
#' @param time Integer vector. At what time indices should predictions be made?
#'  For the default value "model", predictions are made for every time present
#' in the model data.
#'
#' @export
#' @describeIn strv_predict Predictions will be made for all raster cells
#'   whose value are not NA. If the raster has no values, then predictions will
#'   be made at every raster cell. Raster predictions are not treated as areal
#'   data, instead point predictions are made at the midpoint of each raster cell.
#'   The value of the midpoint prediction is taken as the prediction for that cell.
#'   Returns a \code{stars} object whose first two dimensions are the raster
#'   geometry of \code{new_data}, the third dimension is the time index given
#'   in \code{time}, and the fourth dimension is the response variable.
setMethod(f = "strv_predict",
          signature = c("starve","RasterLayer"),
          definition = function(x,new_data,covariates,time="model") {
  # Convert raster to sf
  uniq_prediction_points<- sf::st_as_sf(raster::rasterToPoints(new_data,spatial=TRUE))
  uniq_prediction_points<- uniq_prediction_points[,attr(uniq_prediction_points,"sf_column")]
  if( identical(time,"model") ) {
    time<- seq(min(dat(x)[,time_name(x),drop=TRUE]),max(dat(x)[,time_name(x),drop=TRUE]))
  } else {
    time<- seq(min(time),max(time))
  }
  prediction_points<- sf::st_sf(data.frame(time = rep(time,each=nrow(uniq_prediction_points)),
                                           geom = rep(sf::st_geometry(uniq_prediction_points),length(time))))
  colnames(prediction_points)[[1]]<- time_name(x)

  # Dispatch predictions based on if covariates are in the model and
  # if they are supplied
  covar_names<- names_from_formula(formula(settings(x)))
  if( missing(covariates) && (length(covar_names) == 0) ) {
    pred<- strv_predict(x,prediction_points)
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Check the names of your raster covariates.")
  } else {
    covar_points<- sf_from_raster_list(covariates,time_name=time_name(x))
    colnames(prediction_points)[[2]]<- attr(covar_points,"sf_column")
    sf::st_geometry(prediction_points)<- attr(covar_points,"sf_column")
    prediction_points<- do.call(rbind,Map(
      sf::st_join,
      split(prediction_points,prediction_points[,time_name(x),drop=TRUE]),
      split(covar_points,covar_points[,time_name(x),drop=TRUE]),
      suffix = lapply(unique(covar_points[,time_name(x),drop=TRUE]),function(t) return(c("",".a")))
    ))

    prediction_points[,paste0(time_name(x),".a")]<- NULL
    pred<- strv_predict(x,prediction_points)
  }

  # Convert predictions to stars object
  # should have dimension 4 : x coordinate, y coordinate, time, variable
  var_stars<- lapply(seq_along(n_response(formula(x))),function(i) {
    pred_sf_list<- cbind(do.call(cbind,values(pred)[,,i]),locations(pred))
    pred_sf_list<- lapply(split(pred_sf_list,pred_sf_list[,time_name(x),drop=TRUE]),
                          stars::st_rasterize,
                          template = stars::st_as_stars(new_data))
    pred_stars<- do.call(c,c(pred_sf_list,list(along = time_name(x))))
    return(pred_stars)
  })

  if( length(var_stars) == 1 ) {
    pred_stars<- do.call(c,c(var_stars,var_stars,list(along = "variable")))
    pred_stars<- pred_stars[,,,,1,drop=FALSE]
  } else {
    pred_stars<- do.call(c,c(var_stars,list(along = "variable")))
  }
  pred_stars[[time_name(x)]]<- NULL
  attr(pred_stars,"dimensions")[[time_name(x)]]$delta<- 1
  attr(pred_stars,"dimensions")[[time_name(x)]]$offset<- min(time)
  attr(pred_stars,"dimensions")[["variable"]]$values<- response_names(formula(x))
  names(pred_stars)[1:6]<- names(values(pred))[1:6]

  return(pred_stars)
})



#' @rdname strv_predict
#'
#' @name predict_w
#'
#' @section Random effect predictions:
#' Predictions of spatio-temporal random effects.
#'
#' If there are prediction times that are outside the range of times of the
#'   model, then persistent graph random effects are added to the model to cover
#'   these additional times. Then a prediction graph is created which describes
#'   which random effect locations (including both persistent graph and transient
#'   graph locations) are used as nearest neighbours when finding the predictive
#'   distribution for the spatio-temporal random effect at each prediction location.
#'
#' We then add the predictive distributions for the prediction random effects
#'   to the model likelihood function. The predicted values and standard errors
#'   for the random effect are found by optimizing the augmented likelihood function
#'   evaluated at the model parameter values. Note that the standard errors for
#'   the predicted random effects take into account uncertainty in the model
#'   parameter estimates.
NULL
# #' Predict random effects from likelihood function
# #'
# #' @param x A starve object
# #' @param predictions A long_stars object
# #' @param dist_tol A small number so that prediction variances are not
# #'   computationally singular
# #'
# #' @return An \code{sf} object with predictions for random effects (w) and
# #'   their standard errors.
#' @noRd
predict_w<- function(x,
                      predictions,
                      dist_tol = 0.00001,
                      ...) {
  # Add random effects for times not present in the original model,
  # only added to pass to TMB
  pred_times<- unique(locations(predictions)[,time_name(x),drop=TRUE])
  x<- add_random_effects_by_time(x,pred_times)

  dag<- construct_prediction_graph(
    pred = predictions,
    model = x
  )

  # Prepare input for TMB, TMB_in(x) doesn't take care of pred_* things
  TMB_input<- TMB_in(x)
  TMB_input$data$pred_edges<- edges(idxR_to_C(dag))
  TMB_input$data$pred_dists<- distances(dag)
  TMB_input$data$pred_t<- c(locations(predictions)[,time_name(x),drop=TRUE]- min(stars::st_get_dimension_values(pg_re(x),time_name(x))))

  TMB_input$para$pred_re<- values(predictions)$w
  intersects<- do.call(c,lapply(edges(dag),function(e) return(length(e$from) == 1)))
  TMB_input$map$pred_re<- array(FALSE,dim=dim(TMB_input$para$pred_re))
  TMB_input$map$pred_re[intersects,]<- TRUE
  TMB_input$map$pred_re<- logical_to_map(TMB_input$map$pred_re)

  # Create the TMB object and evaluate it at the ML estimates
  obj<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "starve_TMB",
    silent = TRUE,
    ...
  )
  obj$fn(opt(TMB_out(x))$par)


  # Get standard errors at ML estimates
  sdr<- TMB::sdreport(obj,
                      par.fixed = opt(TMB_out(x))$par,
                      hessian.fixed = parameter_hessian(tracing(x)),
                      getReportCovariance = FALSE)

  est<- as.list(sdr,"Estimate")
  se<- as.list(sdr,"Std. Error")
  values(predictions)$w<- est$pred_re
  values(predictions)$w_se<- se$pred_re

  s<- do.call(c,lapply(edges(dag),function(e) return(e$from[[1]])))
  t<- time_from_formula(formula(x),locations(predictions))-min(stars::st_get_dimension_values(pg_re(x),time_name(x)))+1
  std_tg_t<- time_from_formula(formula(x),locations(tg_re(x))) - min(stars::st_get_dimension_values(pg_re(x),time_name(x)))+1
  for( i in seq(nrow(locations(predictions))) ) {
    if( !intersects[[i]] ) {
      next;
    } else {}
    if( s[[i]] <= dim(pg_re(x))[[1]] ) {
      # Take re from persistent graph -- be sure to use from est so we get the extra years before/after data
      values(predictions)$w[i,]<- est$pg_re[s[[i]],t[[i]],]
      values(predictions)$w_se[i,]<- se$pg_re[s[[i]],t[[i]],]
    } else {
      # Take re from transient graph
      values(predictions)$w[i,]<- values(tg_re(x))$w[std_tg_t == t[[i]],,drop=FALSE][s[[i]]-dim(pg_re(x))[[1]],]
      values(predictions)$w_se[i,]<- values(tg_re(x))$se[std_tg_t == t[[i]],,drop=FALSE][s[[i]]-dim(pg_re(x))[[1]],]
    }
  }

  return(predictions)
}




#' @rdname strv_predict
#'
#' @name predict_linear
#'
#' @section Linear predictions:
#' Predictions of response mean before applying the link function.
#'
#' The predicted value for the linear predictor is given by X*beta + w where
#'   X is the covariate value for the prediction location, beta is the vector of
#'   model regression coefficients, and w is the predicted random effect value
#'   for the prediction location.
#'
#' The standard error for the prediction is given by sqrt(X*SE*X^T + w_se^2) where
#'   SE is the parameter estimate covariance matrix for the regression coefficients
#'   and w_se is the standard errors for the random effect prediction. Note that
#'   these standard errors assume that the estimators for the regression coeffiecients
#'   are independent from the random effects, which may not be true if the covariates
#'   are spatially structured due to an effect called spatial confounding.
NULL
# #' Update random effect predictions to include covariates (link scale)
# #'
# #' @param x A starve object. If se = TRUE, should be a starve object.
# #' @param predictions A long_stars object, typically the output of predict_w.
# #'   Should contain covariate data in slot 'locations'
# #' @param se Should standard errors be calculated?
# #'
# #' @return A long_stars object with predictions and standard errors for the linear term.
#' @noRd
predict_linear<- function(x,
                          predictions,
                          se = TRUE) {
  ### No intercept since it's already taken care of in predict_w
  if( length(names_from_formula(formula(x))) == 0 ) {
    # If there are no covariates, there's nothing to do
    values(predictions)$linear<- values(predictions)$w
    if( se ) { values(predictions)$linear_se<- values(predictions)$w_se }
      else { values(predictions)$linear_se[]<- NA }
  } else {
    # Create design matrix from covariates
    design<- as.matrix(mean_design_from_formula(formula(x),
                                                 locations(predictions)))

    # Create linear predictions
    for( v in seq(n_response(formula(x))) ) {
      beta<- fixed_effects(x)[[v]][,"par"]
      names(beta)<- rownames(fixed_effects(x)[[v]])
      values(predictions)$linear[,v]<- design %*% beta + cbind(values(predictions)$w[,v])
    }

    if( se ) {
      for( v in seq(n_response(formula(x))) ) {
        # Create parameter covariance estimate for fixed effects
        par_cov<- matrix(0,ncol=nrow(fixed_effects(x)[[v]]),nrow=nrow(fixed_effects(x)[[v]]))
        colnames(par_cov)<- rownames(par_cov)<- row.names(fixed_effects(x)[[v]])

        # Fill in the covariance matrix with the standard errors for fixed effect
        # coefficients.
        parameter_covariance<- parameter_covariance(tracing(x))
        par_idx<- rownames(parameter_covariance) %in% c("beta")
        par_sdreport<- parameter_covariance[par_idx,par_idx,drop=FALSE] # Drop = FALSE to keep matrix
        vlengths<- do.call(c,lapply(fixed_effects(x),nrow))
        vstarts<- c(1,1+cumsum(vlengths))
        vpar_idx<- seq(vstarts[[v]],vstarts[[v]]+vlengths[[v]]-1) # mean pars for variable v
        # Keep fixed fixed effects with an standard error of 0
        par_idx<- names(beta)[fixed_effects(parameters(x))[[v]][,"fixed"] == FALSE]
        par_cov[par_idx,par_idx]<- par_sdreport[vpar_idx,vpar_idx]

        # The commented and uncommented methods are the same
        # linear_se<- sqrt(diag(design %*% par_cov %*% t(design)) + w_predictions$w_se^2)
        values(predictions)$linear_se[,v]<- sqrt(rowSums((design %*% par_cov) * design) + values(predictions)$w_se[,v]^2)
      }
    } else {
      values(predictions)$linear_se[]<- NA
    }
  }
  return(predictions)
}




#' @rdname strv_predict
#'
#' @name predict_response
#'
#' @section Response predictions:
#' Predictions of response mean after applying the link function.
#'
#' The predicted value for the response mean is the linear predictor transformed
#'   to the scale of data by applying the link function. The standard error for
#'   the prediction is obtained via the delta method using a second-order Taylor
#'   approximation.
NULL
# #' Update predictions on link scale to the response scale
# #'
# #' A second-order Taylor approximation (delta method) is used
# #'
# #' @param x A starve object. If se = TRUE, should be a fitted starve object.
# #' @param predictions A data.frame, typically the output of predict_linear.
# #'   Must contain at least the columns linear and linear_se.
# #' @param se Should standard errors be calculated?
# #'
# #' @return A data.frame including the supplied linear_predictions and the predictions
# #'   on the response scale with standard errors
#' @noRd
predict_response<- function(x,
                             predictions,
                             se = TRUE) {
  for( v in seq(n_response(formula(x))) ) {
    # Just need to specify which link function is used
    # Will get the function and gradient from TMB, evaluated at
    # linear and linear_se
    data<- list(
      model = "family",
      link_code = link_to_code(link_function(x)[[v]])
    )
    para<- list(x = 0)
    link_function<- TMB::MakeADFun(
      data = data,
      para = para,
      DLL = "starve_TMB",
      silent = TRUE
    )

    values(predictions)$response[,v]<- Vectorize(link_function$fn,SIMPLIFY = "array")(values(predictions)$linear[,v])

    if( se ) {
      # Standard error =  f'(linear)^2*linear_se^2 + 0.5 * f''(linear)^2*linear_se^4
      second_order_se<- Vectorize(function(linear,linear_se) {
        se<- sqrt(link_function$gr(linear)^2*linear_se^2
          + 0.5*link_function$he(linear)^2*linear_se^4)
        return(as.numeric(se))
      },SIMPLIFY = "array")
      values(predictions)$response_se[,v]<- second_order_se(values(predictions)$linear[,v],
                                                            values(predictions)$linear_se[,v])
    } else {
      values(predictions)$response_se[]<- NA
    }
  }

  return(predictions)
}
