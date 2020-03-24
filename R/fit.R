#' @include classes.R access_staRVe.R access_TMB_out.R formula.R
NULL

#' Fit a TMB model
#'
#' @param Input A list with three components, the output of \code{\link{prepare_staRVe_input}}.
#'  The first element stores the raw TMB input, the second stores \code{sf} objects
#'  for Observation and Process, and the third stores model settings.
#' @param silent Suppress optimization and residual tracing information?
#' @param runOrderings Logical value. If true, runs TMB::runSymbolicAnalysis
#'  on the MakeADFun TMB object. Requires a special install of TMB,
#'  see \code{?TMB::runSymbolicAnalysis}.
#' @param ... Optional parameters to be passed to TMB::MakeADFun, TMB::sdreport,
#'  or nlminb.
#'
#' @return An object of class \code{staRVe} containing the model fit.
#'
#' @export
fit_staRVe<- function(Input,silent=F,runOrderings=F,...) {
    TMB_input<- Input[[1]]
    sf_data<- Input[[2]]
    settings<- Input[[3]]

    data<- TMB_input$data
    data$ys_edges<- Rdag_to_Cdag(data$ys_edges)
    data$ws_edges<- Rdag_to_Cdag(data$ws_edges)

    pars<- TMB_input$pars
    rand<- TMB_input$rand
    map<- TMB_input$map
    DLL<- "staRVe"

    obj<- TMB::MakeADFun(data=data,
                         para=pars,
                         random=rand,
                         map=map,
                         DLL=DLL,
                         silent=silent,
                         ...)

    if( runOrderings == T ) {
        TMB::runSymbolicAnalysis(obj)
    } else {}

    system.time({
      opt<- nlminb(obj$par,obj$fn,obj$gr)
    }) -> settings$opt_time
    system.time({
      settings$hess<- numDeriv::jacobian(obj$gr,
                                         opt$par,
                                         method = "simple")
      settings$hess<- as.matrix(Matrix::forceSymmetric(settings$hess))
      settings$par_cov<- solve(settings$hess)
      rownames(settings$par_cov)<- colnames(settings$par_cov)<- names(opt$par)
    }) -> settings$hessian_time
    system.time({
      sdr<- TMB::sdreport(obj,
                          par.fixed = opt$par,
                          hessian.fixed = settings$hess,
                          getReportCovariance=F,...)
    }) -> settings$sdr_time

    TMB_out<- new("TMB_out",
                  obj=obj,
                  opt=opt,
                  sdr=sdr,
                  symbolicAnalysis=runOrderings,TMB_in=TMB_input)
    fit<- staRVe(TMB_out,
                 Observation_sf=sf_data$Observation,
                 Process_sf=sf_data$Process,
                 settings = settings)
    rownames(observation(fit))<- NULL
    rownames(process(fit))<- NULL

    observation<- sf:::cbind.sf(data.frame(w = 0,w_se = 0),observation(fit))
    obs_dag<- TMB_in(fit)$data$ys_edges

    resp_w_idx<- 1
    sdout<- sdreport(fit)
    resp_w_rows<- grep(paste0("^","resp_w"),rownames(sdout))
    resp_w<- sdout[resp_w_rows,]
    colnames(resp_w)<- c("w","w_se")

    for( i in seq(nrow(observation)) ) {
      if( length(obs_dag[[i]]) == 1 ) {
        this_time<- observation[i,settings(fit)$time_column,drop=T]
        w<- process(fit)[process(fit)[,settings(fit)$time_column,drop=T] == this_time,]
        observation[i,c("w","w_se")]<- w[obs_dag[[i]],c("w","w_se"),drop=T]
      } else {
        observation[i,c("w","w_se")]<- resp_w[resp_w_idx,c("w","w_se")]
        resp_w_idx<- resp_w_idx+1
      }
    }
    observation<- .predict_linear(fit,observation,observation)
    observation<- .predict_response(fit,observation)
    observation[,1:6]<- observation[,c(5,6,3,4,1,2),drop=T]
    colnames(observation)[1:6]<- colnames(observation)[c(5,6,3,4,1,2)]
    observation(fit)<- observation


    return(fit)
}


#' Compute residuals for a staRVe model
#'
#' @param object A staRVe object.
#'
#' @export
residuals<- function(object) {
    distribution_code<- obj(object)$env$data$distribution_code

    sf_obj<- observation(object)[,c(settings(object)$time_column,
                                    attr(observation(object),"sf_column"))]

    if( distribution_code == 0 ) { # Normal
      warning("Residuals not supported for this response distribution.")
      pred<- data.frame(residual = numeric(nrow(sf_obj)))
    } else if ( distribution_code %in% c(1,2) ) { # Poisson / Neg. Binom
      max_obs<- as.vector(obj(object)$env$data[["y"]])
      max_obs<- as.integer(round(2*max(max_obs)))
      pred<- TMB::oneStepPredict(obj(object),
                  observation.name = "obs_y",
                  data.term.indicator="keep",
                  discrete=T,
                  discreteSupport=c(0,max_obs),
                  method="oneStepGeneric",
                  trace = F)
    } else if ( distribution_code == 3 ) { # Bernoulli
      pred<- TMB::oneStepPredict(obj(object),
                  observation.name = "obs_y",
                  data.term.indicator="keep",
                  discrete=T,
                  discreteSupport=c(0,1),
                  method="oneStepGeneric",
                  trace = F)
    } else {
      warning("Residuals not supported for this response distribution.")
      pred<- data.frame(residual = numeric(nrow(sf_obj)))
    }
    resid<- cbind(pred$residual,sf_obj)
    colnames(resid)[1]<- "residual"

    return(resid)
}
