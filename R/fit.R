#' Fit a \code{staRVe_model} object.
#'
#' @param x A staRVe_model object.
#' @param silent Should tracing information be printed?
#'
#' @return A staRVe_fit object.
#'
#' @export
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
    DLL = "staRVe",
    silent = silent,
    ...
  )

  system.time({
    opt(TMB_out)<- nlminb(obj(TMB_out)$par,
                          obj(TMB_out)$fn,
                          obj(TMB_out)$gr)
  }) -> opt_time(tracing)

  system.time({
    hess<- numDeriv::jacobian(
      obj(TMB_out)$gr,
      opt(TMB_out)$par,
      method = "simple"
    )
    hess<- as.matrix(Matrix::forceSymmetric(hess))
    par_cov<- solve(hess)

    rownames(hess)<-
      colnames(hess)<-
      rownames(par_cov)<-
      colnames(par_cov)<-
      names(opt(TMB_out)$par)

    parameter_hessian(tracing)<- hess
    parameter_covariance(tracing)<- par_cov
  }) -> hess_time(tracing)

  system.time({
    sdr(TMB_out)<- TMB::sdreport(
      obj(TMB_out),
      par.fixed = opt(TMB_out)$par,
      hessian.fixed = parameter_hessian(tracing),
      getReportCovariance = F,
      ...
    )
  }) -> sdr_time(tracing)

  fit<- new("staRVe_fit",
            staRVe_model = x,
            tracing = tracing,
            TMB_out = TMB_out)
  as(fit,"staRVe_model")<- update_staRVe_model(
    x = as(fit,"staRVe_model"),
    y = TMB_out(fit)
  )
  predictions<- dat(observations(fit))
  predictions<- .predict_linear(fit,predictions,predictions)
  predictions<- .predict_response(fit,predictions)

  dat(observations(fit))[,c("linear","linear_se","response","response_se")]<-
    predictions[,c("linear","linear_se","response","response_se"),drop=T]

  return(fit)
})

#' Predict from a \code{staRVe_fit} object.
#'
#' @param x An object of class \code{staRVe_fit}.
#' @param locations Either an object of class \code{sf} containing point geometries,
#'  or an object of class \code{RasterLayer}. This object should not have any
#'  time information, as predictions will be made at each location at every time.
#'  If a \code{RasterLayer} object, predictions will be made for all raster cells
#'  whose value are not NA. If the raster has no values, then predictions will
#'  be made at every cell. Raster predictions are made at the midpoint of each cell.
#' @param covariates Either a data.frame of class \code{sf} or a list of \code{Raster*}
#'  objects, depending on the input type of \code{locations}. If the model has
#'  no covariates, then nothing needs to be supplied.
#'
#'  If \code{locations} is of class \code{sf} with point geometries, then
#'  \code{covariates} should also be of class \code{sf}. The data.frame should contain
#'  columns for each of the covariates, a column for the time index, and a column
#'  of point geometries. For each time unit and prediction point, there should
#'  be a row in the \code{covariates} data.frame, but the rows do not need to be
#'  in the same order as \code{locations}.
#'
#'  If \code{locations} is of class \code{RasterLayer}, then \code{covariates}
#'  should be a list of \code{Raster*} objects. Each \code{Raster*} object should
#'  contain data for one covariate, should have one layer for each time unit,
#'  and should have the same raster geometry as the \code{locations} object. The
#'  layer names of each raster layer should be of the form \code{T####}, where
#'  \code{####} gives the specific time index.
#' @param time What time indices should predictions be made for? If set to "model",
#'  predictions are made for every time present in the model. The supplied time
#'  indices must be a subset of those in the model.
#'
#' @return Either a \code{sf} object or a list of \code{Raster*} objects,
#'  containing predictions (with standard errors) for the spatial field, the linear
#'  predictor on the link scale, and the response.
#'  The return type is the same as the type of input for \code{locations}.
#'
#' @export
#' @rdname staRVe_predict
setMethod(f = "staRVe_predict",
          signature = c("staRVe_fit","sf"),
          definition = function(x,
                                locations,
                                covariates,
                                time = "model",
                                ...) {
  covar_names<- .names_from_formula(formula(settings(x)))
  if( missing(covariates) && (length(covar_names) == 0) ) {
    covariates<- "missing"
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Please check covariate names.")
  } else {  }

  model_times<- unique(random_effects(process(x))[,attr(random_effects(process(x)),"time_column"),drop=T])
  if( identical(time,"model") ) {
    time<- model_times
  } else {}

  predictions<- .predict_w(x,
                           locations = locations,
                           pred_times = time)
  predictions<- .predict_linear(x,
                                predictions,
                                covariates)
  predictions<- .predict_response(x,
                                  predictions)

  predictions[,1:6]<- predictions[,c(5,6,3,4,1,2),drop=T]
  colnames(predictions)[1:6]<- colnames(predictions)[c(5,6,3,4,1,2)]
  attr(predictions,"time_column")<- attr(random_effects(process(x)),"time_column")

  return(predictions)
})
#' @export
#' @rdname staRVe_predict
setMethod(f = "staRVe_predict",
          signature = c("staRVe_fit","RasterLayer"),
          definition = function(x,locations,covariates,...) {
  prediction_points<- sf::st_as_sf(raster::rasterToPoints(locations,spatial=T))
  prediction_points<- prediction_points[,attr(prediction_points,"sf_column")]

  covar_names<- .names_from_formula(formula(settings(x)))
  if( missing(covariates) && (length(covar_names) == 0) ) {
    pred<- staRVe_predict(x,prediction_points,...)
  } else if( missing(covariates) && (length(covar_names) != 0) ) {
    stop("Missing covariates, please supply them.")
  } else if( !all(covar_names %in% names(covariates)) ) {
    stop("Missing some covariates. Check the names of your raster covariates.")
  } else {
    covar_points<- .sf_from_raster_list(covariates,
      time_name=attr(random_effects(process(x)),"time_column"))
    pred<- staRVe_predict(x,prediction_points,covar_points,...)
  }

  pred_by_time<- split(pred,pred[,attr(random_effects(process(x)),"time_column"),drop=T])
  pred_raster_by_time<- lapply(pred_by_time,raster::rasterize,locations)
  pred_raster_by_time<- lapply(pred_raster_by_time,function(raster_time) {
    ID_layer<- 1
    time_layer<- raster::nlayers(raster_time)
    return(raster_time[[-c(ID_layer,time_layer)]])
  }) # Remove ID and time layer

  return(pred_raster_by_time)
})

.predict_w<- function(x,
                      locations,
                      pred_times,
                      dist_tol = 0.00001,
                      ...) {
  random_effects<- random_effects(process(x))
  locations<- unique(locations[,attr(locations,"sf_column")])
  pred_times<- unique(pred_times)

  predictions<- do.call(rbind,lapply(pred_times,function(t) {
    df<- sf:::cbind.sf(data.frame(w = 0,
                                  w_se = NA,
                                  time = t),
                       locations)
    colnames(df)[[3]]<- attr(random_effects,"time_column")
    return(df)
  }))
  attr(predictions,"time_column")<- attr(random_effects,"time_column")

  dag<- construct_obs_dag(
    x = locations,
    y = split(
      random_effects,
      random_effects[,attr(random_effects,"time_column"),drop=T]
    )[[1]],
    settings = settings(x)
  )
  distances(dag)<- lapply(distances(dag),function(x) {
    if( identical(matrix(0),x) ) {
      return(matrix(c(0,dist_tol,dist_tol,0),nrow = 2))
    } else {
      return(x)
    }
  })

  x<- .add_random_effects_by_time(x,pred_times)

  TMB_input<- TMB_in(x)
  TMB_input$data$pred_w_time<- c(predictions[,attr(random_effects,"time_column"),drop=T]
    - min(random_effects(process(x))[,attr(random_effects(process(x)),"time_column"),drop=T]))
  TMB_input$data$pred_ws_edges<- edges(idxR_to_C(dag))
  TMB_input$data$pred_ws_dists<- distances(dag)

  TMB_input$para$pred_w<- predictions$w
  TMB_input$map$pred_w<- NULL

  obj<- TMB::MakeADFun(
    data = TMB_input$data,
    para = TMB_input$para,
    random = TMB_input$rand,
    map = TMB_input$map,
    DLL = "staRVe",
    silent = TRUE,
    ...
  )
  obj$fn(opt(TMB_out(x))$par)

  sdr<- summary(TMB::sdreport(obj,
                      par.fixed = opt(TMB_out(x))$par,
                      hessian.fixed = parameter_hessian(tracing(x)),
                      getReportCovariance = F))
  predictions[,c("w","w_se")] <- sdr[rownames(sdr) == "pred_w",]

  return(predictions)
}

.predict_linear<- function(x,
                           w_predictions,
                           covariates) {
  if( identical(covariates,"missing") ) {
    design<- matrix(1,nrow = nrow(w_predictions))
  } else {
    time_column<- attr(random_effects(process(x)),"time_column")
    covar_names<- .names_from_formula(formula(settings(x)))

    w_predictions<- w_predictions[,!(names(w_predictions) %in% covar_names)]
    w_predictions<- split(w_predictions,
                          w_predictions[,time_column,drop=T])

    covariates<- covariates[,c(covar_names,time_column)]
    covariates<- split(covariates,covariates[,time_column,drop=T])
    covariates<- covariates[names(covariates) %in% names(w_predictions)] # Get same years

    w_predictions<- do.call(rbind,lapply(names(w_predictions), function(t) {
      this_w<- w_predictions[[t]]
      this_covar<- covariates[[t]][,covar_names]
      suppressMessages(return(sf::st_join(this_w,this_covar,left=T)))
    }))

    design<- .mean_design_from_formula(formula(settings(x)),
                                       w_predictions)
    design<- cbind(1,design)
  }

  beta<- opt(TMB_out(x))$par[names(opt(TMB_out(x))$par) %in% c("mu","mean_pars")]

  parameter_covariance<- parameter_covariance(tracing(x))
  par_idx<- rownames(parameter_covariance) %in% c("mu","mean_pars")
  par_cov<- as.matrix(parameter_covariance[par_idx,par_idx])
  linear<- design %*% beta + w_predictions$w

  # The commented and uncommented methods are the same
  # linear_se<- sqrt(diag(design %*% par_cov %*% t(design)) + w_predictions$w_se^2)
  linear_se<- sqrt(rowSums((design %*% par_cov) * design) + w_predictions$w_se^2)

  return(sf:::cbind.sf(linear,linear_se,w_predictions))
}

.predict_response<- function(x,
                             linear_predictions) {
  data<- list(link_code = .link_to_code(
    link_function(parameters(observations(x)))
  ))
  para<- list(x = 0)
  link_function<- TMB::MakeADFun(
    data = data,
    para = para,
    DLL = "family",
    silent = T
  )

  second_order_mean<- function(linear,linear_se) {
    mean<- link_function$fn(linear) + 0.5*link_function$he(linear)*linear_se^2
    return(as.numeric(mean))
  }
  second_order_se<- function(linear,linear_se) {
    se<- sqrt(link_function$gr(linear)^2*linear_se^2
      + 0.5*link_function$he(linear)^2*linear_se^4)
    return(as.numeric(se))
  }
  response<- sapply(seq(nrow(linear_predictions)),function(i) {
    return(second_order_mean(linear_predictions[i,"linear",drop=T],
                             linear_predictions[i,"linear_se",drop=T]))
  })
  response_se<- sapply(seq(nrow(linear_predictions)),function(i) {
    return(second_order_se(linear_predictions[i,"linear",drop=T],
                           linear_predictions[i,"linear_se",drop=T]))
  })
  predictions<- sf:::cbind.sf(response,response_se,linear_predictions)
  return(predictions)
}
