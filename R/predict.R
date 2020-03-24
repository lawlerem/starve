#' @include classes.R access_staRVe.R access_TMB_out.R fit.R formula.R
NULL

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
setGeneric(name = "predict_staRVe",
           def = function(x,locations,...) standardGeneric("predict_staRVe")

)
#' @param n_neighbours The maximum number of parents each node should have.
#'  If set to "model", then this number is taken from the model settings.
#' @param p_far_neighbours What percent of neighbours should be randomly selected?
#'  The default is 0. If set to "model", then this number is taken from the model settings.
#'
#' @export
#' @rdname predict_staRVe
setMethod(f = "predict_staRVe",
          signature = c("staRVe","sf"),
          definition = function(x,
                                locations,
                                covariates,
                                n_neighbours = "model",
                                p_far_neighbours = 0,
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

    if( identical(p_far_neighbours,"model") ) {
      p_far_neighbours<- settings(x)$p_far_neighbours
    } else {}

    max_dist<- Inf
    max_dist<- units::set_units(max_dist,settings(x)$distance_units,mode="standard")
    max_dist<- units::set_units(max_dist,"m")

    model_times<- unique(process(x)[,settings(x)$time_column,drop=T])
    if( identical(time,"model") ) {
      pred_times<- model_times
    } else {
      pred_times<- intersect(model_times,time)
    }

    predictions<- .predict_w(x,
                             locations = locations,
                             n_neighbours = n_neighbours,
                             p_far_neighbours = p_far_neighbours,
                             max_dist = max_dist,
                             pred_times = pred_times)
    colnames(predictions)[colnames(predictions) == "time"] <- settings(x)$time_column

    predictions<- .predict_linear(x,
                                  predictions,
                                  covariates)
    predictions<- .predict_response(x,predictions)
    predictions[,1:6]<- predictions[,c(5,6,3,4,1,2),drop=T]
    colnames(predictions)[1:6]<- colnames(predictions)[c(5,6,3,4,1,2)]

    return(predictions)
})


.predict_w<- function(x,
                      locations,
                      n_neighbours,
                      p_far_neighbours,
                      max_dist,
                      pred_times,
                      dist_tol = 0.00001,
                      ...) {
  locations<- locations[,attr(locations,"sf_column")]
  process_locs<- process(x)
  process_times<- process_locs[,settings(x)$time_column,drop=T]
  process_locs<- process_locs[process_times == pred_times[[1]],]
  dag<- construct_obs_dag(x = locations,
                          y = process_locs,
                          n_neighbours = n_neighbours,
                          p_far_neighbours = p_far_neighbours,
                          check_intersection = T,
                          max_dist = max_dist,
                          distance_units = settings(x)$distance_units,
                          silent = T)
  pred_ws_dag<- dag[[1]]
  pred_ws_dists<- dag[[2]]

  pred_ws_dists<- lapply(pred_ws_dists,function(x) {
    if( identical(matrix(0),x) ) {
      return(matrix(c(0,dist_tol,dist_tol,0),nrow=2))
    } else {
      return(x)
    }
  })

  locations<- do.call(rbind,lapply(pred_times,function(time) {
    sf:::cbind.sf(time,locations)
  }))
  pred_times<- locations$time

  obj<- obj(x)
  obj$env$data$pred_w_time<- pred_times - min(process(x)[,settings(x)$time_column,drop=T])
  obj$env$data$pred_ws_edges<- Rdag_to_Cdag(pred_ws_dag)
  obj$env$data$pred_ws_dists<- pred_ws_dists
  obj$env$parameters$pred_w<- numeric(length(pred_times))

  data<- obj$env$data
  pars<- obj$env$parameters
  rand<- c("resp_w","proc_w","pred_w")
  # map<- obj$env$map # Don't need this, it's taken care of in obj$env$parameters
  DLL<- "staRVe"

  obj<- TMB::MakeADFun(data = data,
                       para = pars,
                       random = rand,
                       # map = map,
                       DLL = DLL,
                       silent = T)
  obj$fn(opt(x)$par)

  sdr<- TMB::sdreport(obj,
                      par.fixed = opt(x)$par,
                      hessian.fixed = settings(x)$hess,
                      getReportCovariance=F)

  TMB_out<- new("TMB_out",
                obj = obj,
                opt = opt(x),
                sdr = sdr,
                symbolicAnalysis = symbolicAnalysis(x),
                TMB_in = list())
  predictions<- get_geo_vars(TMB_out,
                             var = "pred",
                             locations,
                             get_sd = T)
  colnames(predictions)[1:2]<- c("w","w_se")
  rownames(predictions)<- NULL
  return(predictions)
}

.predict_linear<- function(x,
                           w_predictions,
                           covariates) {
  if( identical(covariates,"missing") ) {
    design<- matrix(1,nrow = nrow(w_predictions))
  } else {
    covar_names<- .names_from_formula(settings(x)$formula)

    w_predictions<- w_predictions[,!(names(w_predictions) %in% covar_names)]
    w_predictions<- split(w_predictions,w_predictions[,settings(x)$time_column,drop=T])

    covariates<- covariates[,c(covar_names,settings(x)$time_column)]
    covariates<- split(covariates,covariates[,settings(x)$time_column,drop=T])
    covariates<- covariates[names(covariates) %in% names(w_predictions)]

    do.call(rbind,lapply(names(w_predictions), function(time) {
      this_w<- w_predictions[[time]]
      this_covar<- covariates[[time]][,covar_names]
      suppressMessages(joined<- sf::st_join(this_w,this_covar,left=T))
      return(joined)
    })) -> w_predictions

    design<- .mean_design_from_formula(settings(x)$formula,
                                              w_predictions)
    design<- cbind(1,design)
  }
  beta<- opt(x)$par[names(opt(x)$par) %in% c("mu","mean_pars")]

  parameter_covariance<- settings(x)$par_cov
  par_idx<- rownames(parameter_covariance) %in% c("mu","mean_pars")
  par_cov<- as.matrix(parameter_covariance[par_idx,par_idx])

  linear<- design %*% beta + w_predictions$w
  # linear_se<- sqrt(diag(design %*% par_cov %*% t(design)) + w_predictions$w_se^2)
  # The method below is equivalent to the commented version
  linear_se<- sqrt(rowSums((design %*% par_cov) * design) + w_predictions$w_se^2)

  return(sf:::cbind.sf(linear,linear_se,w_predictions))
}

.predict_response<- function(x,
                             linear_predictions) {
  data<- list(link_code = settings(x)$link_code)
  pars<- list(x = 0)
  link_function<- TMB::MakeADFun(data = data,
                                 para = pars,
                                 DLL = "family",
                                 silent = T)
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
