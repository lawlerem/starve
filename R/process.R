#' @include classes.R getset.R generics.R utility.R dag.R long_stars.R process_parameters.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param time_effects A stars object
#' @param pg_re A stars object, random effects for the persistent graph
#' @param tg_re A stars object, random effects for the transient graph
#' @param persistent_graph A dag object
#' @param transient_graph A dag object
#' @param parameters A process_parameters object
#'
#' @rdname starve-construct
setMethod(
  f = "initialize",
  signature = "process",
  definition = function(.Object,
                        time_effects = stars::st_as_stars(
                          list(w = array(0,dim=c(1,1)),
                               se = array(0,dim=c(1,1))),
                          dimensions = stars::st_dimensions(time=0,variable="y")
                        ),
                        pg_re = stars::st_as_stars(
                          list(w = array(0,dim=c(1,1,1)),
                               se = array(0,dim=c(1,1,1))),
                          dimensions = stars::st_dimensions(
                            geom = sf::st_sfc(sf::st_point(c(0,0))),
                            time = 0,
                            variable = "y"
                          )
                        ),
                        tg_re = new("long_stars"),
                        persistent_graph = new("dag"),
                        transient_graph = new("dag"),
                        parameters = new("process_parameters")) {
    time_effects(.Object)<- time_effects
    pg_re(.Object)<- pg_re
    tg_re(.Object)<- tg_re
    persistent_graph(.Object)<- persistent_graph
    transient_graph(.Object)<- transient_graph
    parameters(.Object)<- parameters

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
#' @describeIn process_class Get temporal random effects
setMethod(f = "time_effects",
          signature = "process",
          definition = function(x) return(x@time_effects)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn process_class Set temporal random effects
setReplaceMethod(f = "time_effects",
                 signature = "process",
                 definition = function(x,value) {
  x@time_effects<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn process_class Get spatio-temporal random effects for persistent graph
setMethod(f = "pg_re",
          signature = "process",
          definition = function(x) return(x@pg_re)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn process_class Set spatio-temporal random effects for persistent graph
setReplaceMethod(f = "pg_re",
                 signature = "process",
                 definition = function(x,value) {
  x@pg_re<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn process_class Get spatio-temporal random effects for transient graph
setMethod(f = "tg_re",
          signature = "process",
          definition = function(x) return(x@tg_re)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn process_class Set spatio-temporal random effects for transient graph
setReplaceMethod(f = "tg_re",
                 signature = "process",
                 definition = function(x,value) {
  x@tg_re<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn process_class Get persistent graph
setMethod(f = "persistent_graph",
          signature = "process",
          definition = function(x) return(x@persistent_graph)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn process_class Set persistent graph (for internal use only)
setReplaceMethod(f = "persistent_graph",
                 signature = "process",
                 definition = function(x,value) {
  x@persistent_graph<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn process_class Get transient graph
setMethod(f = "transient_graph",
          signature = "process",
          definition = function(x) return(x@transient_graph)
)
#' @param x An object
#' @param value A replacement value
#'
#' @describeIn process_class Set transient graph (for internal use  only)
setReplaceMethod(f = "transient_graph",
                 signature = "process",
                 definition = function(x,value) {
  x@transient_graph<- value
  return(x)
})

#' @param x An object
#'
#' @export
#' @describeIn process_class Get parameters as a process_parameters object
setMethod(f = "parameters",
          signature = "process",
          definition = function(x) return(x@parameters)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn process_class Set parameters using a new process_parameters
#'   object. Not the recommended way to modify specific parameter values, instead see the
#'   package vignette.
setReplaceMethod(f = "parameters",
                 signature = "process",
                 definition = function(x,value) {
  x@parameters<- value
  return(x)
})




#' @param x An object
#'
#' @export
#' @describeIn process_class Get list containing the persistent graph random
#'   effects and the transient graph random effects.
setMethod(f = "random_effects",
          signature = "process",
          definition = function(x) {
  return(list(pg_re = pg_re(x),
              tg_re = tg_re(x)))
})



###############
###         ###
### Utility ###
###         ###
###############


#' @param x An sf object
#' @param y A process object
#'
#' @describeIn create_graph_idx Locations in the sf object x will be given an
#'   index pointing to either a persistent graph or transient graph location of y.
setMethod(
  f = "create_graph_idx",
  signature = c("sf","process"),
  definition = function(x,y,settings) {
    pg_s<- locations_from_stars(pg_re(y))
    tg_s<- locations(tg_re(y)) # Should also have a time column
    tg_t<- time_from_formula(formula(settings),tg_s)

    # Get persistent graph locations
    v<- sf::st_equals(x,pg_s)
    v<- do.call(c,lapply(v,function(x) if(length(x)==0) {return(NA)} else {return(x[[1]])}))

    # Get transient graph locations
    splitx<- split(x[is.na(v),],time_from_formula(formula(settings),x[is.na(v),]))
    splitv<- lapply(splitx,function(t_x) {
      vv<- sf::st_equals(t_x,tg_s[tg_t==time_from_formula(formula(settings),t_x)[[1]],])
      vv<- do.call(c,lapply(vv,function(x) if(length(x)==0) {return(NA)} else {return(x[[1]])}))
      vv<- vv+nrow(pg_s)
      return(vv)
    })
    v[is.na(v)]<- do.call(c,splitv)

    if( any(is.na(v)) ) {
      stop("Some data locations do not coincide with any random effect locations. Try re-creating the process object.")
    } else {}

    return(v)
})
