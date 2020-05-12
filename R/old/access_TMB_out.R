#' @include classes.R generics.R access_staRVe.R
NULL


###########################
###                     ###
###  Construct_TMB_out  ###
###                     ###
###########################

#' @details The \code{initialize} function is not meant to be used by the user,
#'  use \code{TMB_out} instead.
#'
#' @export
#' @rdname TMB_out
setMethod(
    f = "initialize",
    signature = "TMB_out",
    definition = function(.Object,
                          obj = list(),
                          opt = list(),
                          sdr = structure(numeric(0),class="sdreport"),
                          symbolicAnalysis = logical(0),
                          TMB_in = list()) {
        obj(.Object)<- obj
        opt(.Object)<- opt
        sdr(.Object)<- sdr
        symbolicAnalysis(.Object)<- symbolicAnalysis
        TMB_in(.Object)<- TMB_in

        return(.Object)
    }
)





#######################
###                 ###
###  Access_TMB_out ###
###                 ###
#######################

#' Get or set slots from an object of class \code{TMB_out}.
#'
#' @param x An obejct of class \code{TMB_out}.
#' @param value A replacement value.
#'
#' @family Access_TMB_out
#' @name Access_TMB_out
NULL

#' @export
setMethod(f = "obj",
          signature = "TMB_out",
          definition = function(x) return(x@obj)
)
#' @export
setReplaceMethod(f = "obj",
                 signature = "TMB_out",
                 definition = function(x,value) {
                     x@obj<- value
                     return(x)
                 }
)



#' @export
setMethod(f = "opt",
          signature = "TMB_out",
          definition = function(x) return(x@opt)
)
#' @export
setReplaceMethod(f = "opt",
                 signature = "TMB_out",
                 definition = function(x,value) {
                     x@opt<- value
                     return(x)
                 }
)



#' @export
setMethod(f = "sdr",
          signature = "TMB_out",
          definition = function(x) return(x@sdr)
)
#' @export
setReplaceMethod(f = "sdr",
                 signature = "TMB_out",
                 definition = function(x,value) {
                     x@sdr<- value
                     return(x)
                 }
)



#' @export
setMethod(f = "symbolicAnalysis",
          signature = "TMB_out",
          definition = function(x) return(x@symbolicAnalysis)
)
#' @export
setReplaceMethod(f = "symbolicAnalysis",
                 signature = "TMB_out",
                 definition = function(x,value) {
                     x@symbolicAnalysis<- value
                     return(x)
                 }
)



#' @export
setMethod(f = "TMB_in",
          signature = "TMB_out",
          definition = function(x) return(x@TMB_in)
)
#' @export
setReplaceMethod(f = "TMB_in",
                 signature = "TMB_out",
                 definition = function(x,value) {
                     x@TMB_in<- value
                     return(x)
                 }
)



#' @export
setMethod(f = "report",
          signature = "TMB_out",
          definition = function(x) return(obj(x)$report())
)



#' @export
setMethod(f = "sdreport",
          signature = "TMB_out",
          definition = function(x) return(summary(sdr(x)))
)



#' @export
setMethod(f = "get_working_pars",
          signature = "TMB_out",
          definition = function(x) {
  sdr<- sdreport(x)
  working_par_rows<- grep("^working_par_",rownames(sdr))
  working_pars<- sdr[working_par_rows,]
  rownames(working_pars)<- sub("working_par_","",rownames(working_pars))
  colnames(working_pars)<- c("par","par_se")

  working_pars<- as.data.frame(working_pars)
  return(working_pars)
})



#' @export
setMethod(f = "get_pars",
          signature = "TMB_out",
          definition = function(x) {
    sdr<- sdreport(x)
    par_rows<- grep("^par_",rownames(sdr))
    pars<- sdr[par_rows,]
    rownames(pars)<- sub("par_","",rownames(pars))
    colnames(pars)<- c("par","par_se")

    mean_design_names<- colnames(TMB_in(x)$data$mean_design)
    rownames(pars)[rownames(pars)=="mean_pars"]<- mean_design_names

    pars<- as.data.frame(pars)
    return(pars)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_parameters",
          signature = "TMB_out",
          definition = function(x) {
    pars<- get_pars(x)

    return(pars)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_geo_vars",
          signature = "TMB_out",
          definition = function(x,var,sf_obj,get_sd=T) {

    var_exp<- paste0(var,"_")

    if( get_sd == T ) {
        sdout<- sdreport(x)

        var_rows<- grep(paste0("^",var_exp),rownames(sdout))
        var_dat<- sdout[var_rows,]
        colnames(var_dat)<- c("var","var_se")
    } else {
        repout<- report(x)
        var_ind<- grep(paste0("^",var_exp),names(repout))
        var_dat<- as.matrix(unlist(repout[var_ind]))
        colnames(var_dat)<- c("var")
    }

    rownames(var_dat)<- sub(var_exp,"",rownames(var_dat))

    sf_obj<- cbind(var_dat,sf_obj)
    return(sf_obj)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_proc",
          signature = "TMB_out",
          definition = function(x,sf_obj) {
    proc<- get_geo_vars(x,"proc",sf_obj,get_sd=T)
    colnames(proc)[1:2]<- c("w","w_se")
    return(proc)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_obs",
          signature = "TMB_out",
          definition = function(x,sf_obj) {
    obs<- get_geo_vars(x,"obs",sf_obj,get_sd=F)
    # obs<- obs[,-2] # Observations don't have standard errors
    colnames(obs)[1]<- c("y")
    return(obs)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_resp",
          signature = "TMB_out",
          definition = function(x,sf_obj) {
    resp<- get_geo_vars(x,"resp",sf_obj,get_sd=F)
    # colnames(resp)[1:2]<- c("response","response_se")
    colnames(resp)[1]<- c("response")
    return(resp)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_observation",
          signature = "TMB_out",
          definition = function(x,sf_obj) {
    # resp<- get_resp(x,sf_obj)

    return(sf_obj)
})



#' @export
#' @rdname Access_TMB_out
setMethod(f = "get_process",
          signature = "TMB_out",
          definition = function(x,sf_obj) {
    proc<- get_proc(x,sf_obj)

    return(proc)
})



#' @export
setMethod("convergence",
          signature = "TMB_out",
          definition = function(x) return(opt(x)$message)
)
