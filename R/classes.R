#' An S4 class to hold the raw TMB output of \code{fit_catch_data}.
#'
#' @family staRVe-classes
#'
#' @slot obj A list. The output of \code{TMB::MakeADFun}.
#' @slot opt A list. The output of \code{nlminb}.
#' @slot sdr An object of class \code{sdreport}. The output of \code{TMB::sdreport(obj)}.
#' @slot symbolicAnalysis A logical value. Was runOrderings used to fit the model?
#'
#' @export
setClass(
    Class = "TMB_out",
    slots = c(
        obj = "list",
        opt = "list",
        sdr = "sdreport",
        symbolicAnalysis = "logical",
        TMB_in = "list"
    )
)


#' An S4 class to hold results from an analysis of RV data.
#'
#' @family staRVe-classe
#'
#' @slot TMB_out An object of class \code{TMB_out} holding the raw TMB output of the model.
#' @slot Observation An \code{sf} object containing data.frame with columns \code{y}, \code{response}, \code{response_se}, and \code{residual}, and an \code{sfc} object containing location data.
#' @slot Process An \code{sf} object containing data.frame with columns \code{w} and \code{w_se}, and an \code{sfc} object containing location data.
#' @slot parameters A data.frame containing the columns \code{par}, and \code{par_se}.
#' @slot convergence A character vector giving the convergence message.
#"
#' @export
setClass(
    Class = "staRVe",
    slots = c(
        observation = "sf",
        process = "sf",
        parameters = "data.frame",
        convergence = "character",
        settings = "list"
    ),
    contains = "TMB_out"
)
