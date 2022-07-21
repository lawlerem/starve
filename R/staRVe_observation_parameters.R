#' @include classes.R getset.R generics.R
NULL

#################
###           ###
### Construct ###
###           ###
#################

#' @param response_distribution Which response distribution to use
#' @param fixed_effects A data.frame
#'
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "staRVe_observation_parameters",
  definition = function(.Object,
                        response_distribution = "gaussian",
                        fixed_effects = list(data.frame(par = numeric(0),
                                                        fixed = numeric(0)))) {
    response_distribution(.Object)<- response_distribution
    # response distribution takes care of response parameters and link function
    fixed_effects(.Object)<- fixed_effects

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
#' @describeIn staRVe_observation_parameters Get response distribution(s)
setMethod(f = "response_distribution",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_distribution)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_observation_parameters Get/set response distribution(s). Run
#'   get_staRVe_distributions("distribution") for valid options. Setting the
#'   the response distribution also overwrites the response parameters and
#'   link function(s).
setReplaceMethod(f = "response_distribution",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  value<- unname(get_staRVe_distributions("distribution")[pmatch(value,get_staRVe_distributions("distribution"),duplicates.ok=TRUE)])
  x@response_distribution<- value

  response_pars<- lapply(value,function(rd) {
    switch(rd,
      gaussian = data.frame(par = 0,
                            se = 0,
                            fixed = FALSE,
                            row.names = "sd"),
      poisson = data.frame(par = numeric(0),
                           se = numeric(0),
                           fixed = logical(0)),
      `negative binomial` = data.frame(par = 0,
                                       se = 0,
                                       fixed = FALSE,
                                       row.names = "overdispersion"),
      bernoulli = data.frame(par = numeric(0),
                             se = numeric(0),
                             fixed = logical(0)),
      gamma = data.frame(par = 0,
                         se = 0,
                         fixed = FALSE,
                         row.names = "sd"),
      lognormal = data.frame(par = 0,
                             se = 0,
                             fixed = FALSE,
                             row.names = "sd"),
      binomial = data.frame(par = numeric(0),
                            se = numeric(0),
                            fixed = logical(0)),
      atLeastOneBinomial = data.frame(par = numeric(0),
                                      se = numeric(0),
                                      fixed = logical(0)),
      compois = data.frame(par = 0,
                           se = 0,
                           fixed = FALSE,
                           row.names = "dispersion"),
      tweedie = data.frame(par = numeric(2),
                           se = numeric(2),
                           fixed = c(FALSE,FALSE),
                           row.names = c("dispersion","power"))
    )
  })
  try(names(response_pars)<- names(response_parameters(x)))
  response_parameters(x)<- response_pars

  link_function(x)<- do.call(c,lapply(value,function(rd) {
    switch(rd,
      gaussian = "identity",
      poisson = "log",
      `negative binomial` = "log",
      bernoulli = "logit",
      gamma = "log",
      lognormal = "log",
      binomial = "logit",
      atLeastOneBinomial = "logit",
      compois = "log",
      tweedie = "log"
    )
  }))

  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn staRVe_observation_parameters Get response distribution parameters
setMethod(f = "response_parameters",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@response_parameters)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_observation_parameters Set response distribution parameters
setReplaceMethod(f = "response_parameters",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@response_parameters<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn staRVe_observation_parameters Get link function(s)
setMethod(f = "link_function",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@link_function)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_observation_parameters set link function(s).  Run
#'   get_staRVe_distributions("link") for valid options.
setReplaceMethod(f = "link_function",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@link_function<- unname(
    get_staRVe_distributions("link")[
      pmatch(value,get_staRVe_distributions("link"),duplicates.ok=TRUE)
    ]
  )
  return(x)
})


#' @param x An object
#'
#' @export
#' @describeIn staRVe_observation_parameters Get fixed effect parameters
setMethod(f = "fixed_effects",
          signature = "staRVe_observation_parameters",
          definition = function(x) return(x@fixed_effects)
)
#' @param x An object
#' @param value A replacement value
#'
#' @export
#' @describeIn staRVe_observation_parameters Set fixed effect parameters
setReplaceMethod(f = "fixed_effects",
                 signature = "staRVe_observation_parameters",
                 definition = function(x,value) {
  x@fixed_effects<- value
  return(x)
})
