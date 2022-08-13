#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param locations An sf object with a time column and point geometries
#' @param var_names A character vector
#'
#' @noRd
setMethod(
    f = "initialize",
    signature = "long_stars",
    definition = function(
      .Object,
      locations = sf::st_sf(
        time = numeric(0),
        geometry = sf::st_sfc(sf::st_point())[-1]
      ),
      var_names = "y") {
  nidx<- max(1,nrow(locations))
  values(.Object)<- stars::st_as_stars(
    list(w = array(0,dim=c(nidx,length(var_names))),
         w_se = array(NA_real_,dim=c(nidx,length(var_names))),
         linear = array(0,dim=c(nidx,length(var_names))),
         linear_se = array(NA_real_,dim=c(nidx,length(var_names))),
         response = array(0,dim=c(nidx,length(var_names))),
         response_se = array(NA_real_,dim=c(nidx,length(var_names)))),
    dimensions = stars::st_dimensions(i=seq(length.out=nidx),variable=var_names)
  )
  locations(.Object)<- locations

  return(.Object)
})



##############
###        ###
### Access ###
###        ###
##############

#' @param x An object
#'
#' @export
#' @describeIn long_stars_class Get values in multi-dimensional array
setMethod(
    f = "values",
    signature = "long_stars",
    definition = function(x) {
  return(x@values)
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn long_stars_class Set values in multi-dimensional array
setReplaceMethod(
    f = "values",
    signature = "long_stars",
    definition = function(x, value) {
  x@values<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn long_stars_class Get locations
setMethod(
    f = "locations",
    signature = "long_stars",
    definition = function(x) {
  return(x@locations)
})
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn long_stars_class Set locations
setReplaceMethod(
    f = "locations",
    signature = "long_stars",
    definition = function(x, value) {
  x@locations<- value
  return(x)
})
