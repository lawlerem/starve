#' @include classes.R generics.R staRVe_tracing.R staRVe_model.R TMB_out.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{staRVe_fit} instead.
#'
#' @export
#' @rdname staRVe_fit
setMethod(
  f = "initialize",
  signature = "staRVe_fit",
  definition = function(.Object,
                        staRVe_model = new("staRVe_model"),
                        tracing = new("staRVe_tracing"),
                        TMB_out = new("TMB_out")) {
    as(.Object,"staRVe_model")<- staRVe_model
    tracing(.Object)<- tracing
    TMB_out(.Object)<- TMB_out

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' Get or set slots from an object of class \code{staRVe_fit}.
#'
#' @param x An object of class \code{staRVe_fit}.
#' @param value A replacement value
#'
#' @family Access_staRVe_fit
#' @name Access_staRVe_fit
NULL

#' @export
setMethod(f = "tracing",
          signature = "staRVe_fit",
          definition = function(x) return(x@tracing)
)
#' @export
setReplaceMethod(f = "tracing",
                 signature = "staRVe_fit",
                 definition = function(x,value) {
  x@tracing<- value
  return(x)
})



#' @export
setMethod(f = "TMB_out",
          signature = "staRVe_fit",
          definition = function(x) return(x@TMB_out)
)
#' @export
setReplaceMethod(f = "TMB_out",
                 signature = "staRVe_fit",
                 definition = function(x,value) {
  x@TMB_out<- value
  return(x)
})



###############
###         ###
### Utility ###
###         ###
###############

#' Fit a \code{staRVe_model} object.
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

  return(fit)
})
