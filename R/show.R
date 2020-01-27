#' @include classes.R access_staRVe.R access_TMB_out.R
NULL

#' Print method for objects of class \code{TMB_out}.
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "TMB_out",
          definition = function(object) {
    cat("A S4 object with slots obj, opt, and sdr.")
    return(invisible())
})

#' Print method for objects of class \code{staRVe}.
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "staRVe",
          definition = function(object) {
    cat("\n")
    cat("Convergence\n")
    cat("-----------\n\n")
    print(convergence(object))
    cat("\n")
    cat("Estimated Process\n")
    cat("-----------------\n\n")
    print(process(object))
    cat("\n")
    cat("Estimated Observations\n")
    cat("----------------------\n\n")
    print(observation(object))
    cat("\n")
    cat("Estimated Parameters\n")
    cat("--------------------\n\n")
    print(parameters(object))

    return(invisible())
})
