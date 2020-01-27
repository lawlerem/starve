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
#' @param ... Optional parameters to be passed to TMB::MakeADFun and TMB::sdreport.
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

    opt<- nlminb(obj$par,obj$fn,obj$gr)
    sdr<- TMB::sdreport(obj,getReportCovariance=F,...)

    TMB_out<- new("TMB_out",obj=obj,opt=opt,sdr=sdr,
                  symbolicAnalysis=runOrderings,TMB_in=TMB_input)
    fit<- staRVe(TMB_out,
                 Observation_sf=sf_data$Observation,
                 Process_sf=sf_data$Process,
                 settings = settings)
    rownames(observation(fit))<- NULL
    rownames(process(fit))<- NULL

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
