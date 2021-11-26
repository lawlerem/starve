#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param locations An sf object with a time column and point geometryies
#'
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "staRVe_predictions",
  definition = function(.Object,
                        locations = sf::st_sf(data.frame(
                            time = numeric(1)
                          ),
                          geometry = sf::st_sfc(sf::st_point())
                        ),
                        var_names = "y") {
    predictions(.Object)<- stars::st_as_stars(
      list(w = array(0,dim=c(nrow(locations),length(var_names))),
           w_se = array(NA,dim=c(nrow(locations),length(var_names))),
           linear = array(0,dim=c(nrow(locations),length(var_names))),
           linear_se = array(NA,dim=c(nrow(locations),length(var_names))),
           response = array(0,dim=c(nrow(locations),length(var_names))),
           response_se = array(NA,dim=c(nrow(locations),length(var_names)))),
      dimensions = stars::st_dimensions(i=seq(nrow(locations)),variable=var_names)
    )
    locations(.Object)<- locations

    return(.Object)
  }
)



##############
###        ###
### Access ###
###        ###
##############

#' @param x An object
#'
#' @export
#' @describeIn staRVe_predictions Get predictions
setMethod(f = "predictions",
          signature = "staRVe_predictions",
          definition = function(x) return(x@predictions)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_predictions Set predictions
setReplaceMethod(f = "predictions",
                 signature = "staRVe_predictions",
                 definition = function(x,value) {
  x@predictions<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn staRVe_predictions Get locations
setMethod(f = "locations",
          signature = "staRVe_predictions",
          definition = function(x) return(x@locations)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_predictions Set locations
setReplaceMethod(f = "locations",
                 signature = "staRVe_predictions",
                 definition = function(x,value) {
  x@locations<- value
  return(x)
})
