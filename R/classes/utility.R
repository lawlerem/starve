#' @include classes.R generics.R
NULL

###
### Miscellaneous functions which have potential to be widely-used
###   within the package.
### Should be organized in alphabetical order.
###



# .

.covariance_to_code<- function(covariance) {
  covar_code<- charmatch(covariance,get_staRVe_distributions("covariance"))
  if( is.na(covar_code) || covar_code == 0 ) {
    stop("Supplied covariance function is not implemented, or matches multiple covariance functions.")
  } else {
    covar_code<- covar_code - 1 # Convert to cpp
  }
}

.distribution_to_code<- function(distribution) {
  distribution_code<- charmatch(distribution,get_staRVe_distributions("distribution"))
  if( is.na(distribution_code) || distribution_code == 0  ) {
    stop("Supplied distribution is not implemented, or matches multiple distributions.")
  } else {
    distribution_code<- distribution_code - 1 # Convert to cpp
  }

  return(distribution_code)
}

.link_to_code<- function(link) {
  link_code<- charmatch(link,get_staRVe_distributions("link"))
  if( is.na(link_code) || link_code == 0 ) {
    stop("Supplied link function is not implemented, or matches multiple link functions.")
  } else {
    link_code<- link_code - 1 # Convert to cpp
  }

  return(link_code)
}





# A





# B





# C





# D





# E





# F





# G

#' Print a list of implemented response distributions and link functions.
#'
#' @param which A character vector containing "distribution", "link", and/or
#'  "covariance". Defaults to all.
#'
#' @return A named character vector giving the implemented distributions,
#'  link functions, and/or covariance functions, depending on the value of the
#'  `which` parameter.
#'
#' @export
get_staRVe_distributions<- function(which = c("distribution","link","covariance")) {
  if( "distribution" %in% which ) {
    distributions<- c("gaussian", # 0
                      "poisson", # 1
                      "negative binomial", # 2
                      "bernoulli", # 3
                      "gamma", # 4
                      "lognormal") # 5
    names(distributions)<- rep("distribution",length(distributions))
  } else { distributions<- character(0) }

  if( "link" %in% which ) {
    links<- c("identity", # 0
              "log", # 1
              "logit") # 2
    names(links)<- rep("link",length(links))
  } else { links<- character(0) }

  if( "covariance" %in% which ) {
    covars<- c("exponential", # 0
               "gaussian", # 1
               "matern") # 2
    names(covars)<- rep("covariance",length(covars))
  } else { covars<- character(0) }

  return(c(distributions,links,covars))
}





# H





# I





# J





# K





# L





# M





# N





# O

#' Sort an \code{sf} object with point geometry by location.
#'
#' The sorting is determined by lexicographic ordering, in order of the columns
#'  of the geometry object. The locations are put in increasing order according
#'  to the first column, and then ties are put in order according to the second
#'  column, and so on. See \code{\link{order}}.
#'
#' @param x An \code{sf} object containing point geometries.
#' @param time If present, a time index to be the primary (slowest running)
#'  sorting term.
#' @param return Should be one of ``sort" or ``order". If ``sort", a sorted copy
#'  of \code{x} is returned. If ``order", a vector of sorted indices is returned.
#'
#' @return Either a copy of \code{x} whose rows as sorted (\code{return='sort'});
#'  or an integer vector of sorted indices (\code{return='order'}).
#'
#' @export
order_by_location<- function(x,time=NULL,return="sort") {
    coords<- as.data.frame(sf::st_coordinates(x))
    if( !is.null(time) ) {coords<- cbind(time,coords)} else {}
    the_order<- do.call(order,as.list(coords))
    if( return == "order" ) {
        return(the_order)
    } else if( return == "sort" ) {
        x<- x[the_order,]
        return(x)
    } else {
        stop("Argument return must be one of `sort' of `order'")
    }
    return(0)
}





# P





# Q





# R





# S





# T





# U





# V





# W





# X





# Y





# Z
