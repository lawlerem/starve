#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @noRd
setMethod(
  f = "initialize",
  signature = "staRVe_process_parameters",
  definition = function(.Object,
                        covariance_function = "exponential",
                        # Taken care of in covariance_function<-
                        # spatial_parameters = data.frame(par = numeric(0),
                        #                                 fixed = numeric(0)),
                        time_parameters = data.frame(par = c(0,0,0),
                                                     se = c(0,0,0),
                                                     fixed = c(F,F,F),
                                                     row.names = c("mu","ar1","sd"))) {
    covariance_function(.Object)<- covariance_function
    # spatial_parameters(.Object)<- spatial_parameters
    time_parameters(.Object)<- time_parameters

    return(.Object)
  }
)


##############
###        ###
### Access ###
###        ###
##############

#' @export
#' @describeIn staRVe_process_parameters Get/set covariance function. Run
#'   get_staRVe_distributions("covariance") for valid covariance functions.
setMethod(f = "covariance_function",
          signature = "staRVe_process_parameters",
          definition = function(x) return(x@covariance_function)
)
#' @export
setReplaceMethod(f = "covariance_function",
                 signature = "staRVe_process_parameters",
                 definition = function(x,value) {
  x@covariance_function<- value
  spatial_parameters(x)<- switch(value,
    exponential = data.frame(par = c(0,0.5),
                             se = c(0,0),
                             fixed = c(F,T),
                             row.names = c("sd","nu")),
    matern32 = data.frame(par = c(0,1.5),
                          se = c(0,0),
                          fixed = c(F,T),
                          row.names = c("sd","nu")),
    matern = data.frame(par = c(0,0.5),
                        se = c(0,0),
                        fixed = c(F,F),
                        row.names = c("sd","nu")),
    gaussian = data.frame(par = c(0,Inf),
                          se = c(0,0),
                          fixed = c(F,T),
                          row.names = c("sd","nu")),
  )
  return(x)
})



#' @export
#' @describeIn staRVe_process_parameters Get/set spatial parameters
setMethod(f = "spatial_parameters",
          signature = "staRVe_process_parameters",
          definition = function(x) return(x@spatial_parameters)
)
#' @export
setReplaceMethod(f = "spatial_parameters",
                 signature = "staRVe_process_parameters",
                 definition = function(x,value) {
  x@spatial_parameters<- value
  return(x)
})



#' @export
#' @describeIn staRVe_process_parameters Get/set time parameters
setMethod(f = "time_parameters",
          signature = "staRVe_process_parameters",
          definition = function(x) return(x@time_parameters)
)
#' @export
setReplaceMethod(f = "time_parameters",
                 signature = "staRVe_process_parameters",
                 definition = function(x,value) {
  x@time_parameters<- value
  return(x)
})
