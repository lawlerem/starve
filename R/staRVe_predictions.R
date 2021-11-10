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
                        )) {
    predictions(.Object)<- stars::st_as_stars(
      list(w = array(0,dim=c(nrow(locations))),
           w_se = array(NA,dim=c(nrow(locations))),
           linear = array(0,dim=c(nrow(locations))),
           linear_se = array(NA,dim=c(nrow(locations))),
           response = array(0,dim=c(nrow(locations))),
           response_se = array(NA,dim=c(nrow(locations)))),
      dimensions = stars::st_dimensions(i=seq(nrow(locations)))
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
setReplaceMethod(f = "locations",
                 signature = "staRVe_predictions",
                 definition = function(x,value) {
  x@locations<- value
  return(x)
})
