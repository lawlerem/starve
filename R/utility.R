#' @include classes.R generics.R
NULL

###
### Miscellaneous functions which have potential to be widely-used
###   within the package.
### Should be organized in alphabetical order.
###



# .

.add_random_effects_by_time<- function(x,times) {
  random_effects<- random_effects(process(x))
  nodes<- split(
    random_effects[,attr(random_effects,"sf_column")],
    random_effects[,attr(random_effects,"time_column"),drop=T]
  )[[1]]
  model_times<- unique(random_effects[,attr(random_effects,"time_column"),drop=T])

  if( min(times) < min(model_times) ) {
    extra_times<- seq(min(times),min(model_times)-1)
    extra_effects<- do.call(rbind,lapply(extra_times,function(t) {
      df<- sf:::cbind.sf(data.frame(w = 0,
                                    se = NA,
                                    fixed = F,
                                    time = t),
                         nodes)
      colnames(df)[[4]]<- attr(random_effects,"time_column")
      return(df)
    }))

    random_effects(process(x))<- rbind(
      extra_effects,
      random_effects(process(x))
    )
  } else {}

  if( max(times) > max(model_times) ) {
    extra_times<- seq(max(model_times)+1,max(times))
    extra_effects<- do.call(rbind,lapply(extra_times,function(t) {
      df<- sf:::cbind.sf(data.frame(w = 0,
                                    se = NA,
                                    fixed = F,
                                    time = t),
                         nodes)
      colnames(df)[[4]]<- attr(random_effects,"time_column")
      return(df)
    }))

    random_effects(process(x))<- rbind(
      random_effects(process(x)),
      extra_effects
    )
  } else {}

  attr(random_effects(process(x)),"time_column")<- attr(random_effects,"time_column")
  return(x)
}

.birdFit<- function() {
  foo<- prepare_staRVe_model(
    cnt~time(year),
    bird_survey,
    distribution="poisson",
    link="log",
    fit=T
  )
  return(foo)
}

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

# x should be a vector
.logical_to_map<- function(x) {
  y<- seq_along(x)
  y[x]<- NA
  y<- as.factor(y)

  return(y)
}

#' Check which covariates are used in a staRVe formula
#'
#' @param x A formula object with terms grouped in a \code{mean(...)} function.
#'
#' @return A character vector giving the names of covariates used in a formula.
.names_from_formula<- function(x) {
  # Remove response and time from formula
  the_terms<- terms(x,specials=c("mean","time","sample.size"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("mean",term.labels,value=T) # Pick out mean(...)
  if( length(the_call) == 0 ) {
    return(character(0))
  } else {}
  new_formula<- paste(the_call,collapse=" + ")
  new_formula<- paste("~",new_formula)
  var_names<- all.vars(formula(new_formula))
  return(var_names)
}

#' Reform a list of \code{Raster*} objects to an \code{sf} data.frame.
#'
#' @param x A list of \code{Raster*} objects.
#' @param time_name A string giving the name of the time variable.
#'
#' @return An \code{sf} data.frame.
.sf_from_raster_list<- function(x,time_name) {
  sf_list<- lapply(x,function(raster_brick) {
    layer_names<- names(raster_brick)
    layer_list<- lapply(layer_names,function(name) {
      numeric_name<- as.numeric(gsub("t","",name,ignore.case=T))
      sf_layer<- sf::st_as_sf(raster::rasterToPoints(raster_brick[[name]],spatial=T))
      sf_layer<- cbind(sf_layer[,name,drop=T],
                       numeric_name,
                       sf_layer[,attr(sf_layer,"st_geometry")])
      colnames(sf_layer)[1:2]<- c("value",time_name)
      return(sf_layer)
    })
    return(do.call(rbind,layer_list))
  })
  sf_list<- lapply(names(x), function(name) {
    colnames(sf_list[[name]])[[1]]<- name
    return(sf_list[[name]])
  })
  unique_times<- lapply(sf_list, function(x) {
    return(unique(x[,time_name,drop=T]))
  })
  unique_times<- sort(unique(do.call(c,unique_times)))

  unique_geoms<- unique(sf_list[[1]][,attr(sf_list[[1]],"st_geometry")])
  # This assumes the initial rasters are the same.


  sf_output<- lapply(unique_times,function(time) {
    year_sf<- cbind(time,unique_geoms)
    var_columns<- lapply(sf_list,function(var) {
      var_year<- var[var[,time_name,drop=T]==time,]
      var_year[,time_name]<- NULL
      suppressMessages(var_year<- sf::st_join(unique_geoms,var_year))
      var_year<- sf::st_drop_geometry(var_year)
      return(var_year)
    })
    var_df<- do.call(cbind.data.frame,var_columns)
    year_sf<- sf:::cbind.sf(var_df,year_sf)
    return(year_sf)
  })
  sf_output<- do.call(rbind,sf_output)
  colnames(sf_output)[colnames(sf_output)=="time"]<- time_name

  return(sf_output)
}





# A





# B





# C

#' Retrieve a spatial covariance function from a formula.
#'
#' @param x A formula object with a space(covariance,nu) function. The covariance
#'   argument must be one those listed in get_staRVe_distributions("covariance").
#'   The `nu' argument is only necessary when using the Matern covariance, and
#'   supplying it fixes the value of nu (the smoothness parameter).
#'
#' @return A list containing the name of the covariance function and the value of nu.
.covariance_from_formula<- function(x,data=data.frame(1)) {
  space<- function(covariance,nu) {
    covar<- pmatch(covariance,get_staRVe_distributions("covariance"))
    covar<- get_staRVe_distributions("covariance")[covar]
    if( missing(nu) ) {
      nu<- NaN
    }
    covariance<- cbind(covar,nu)
    colnames(covariance)<- c("covariance","nu")

    return(covariance)
  }
  the_terms<- terms(x,specials=c("mean","time","space"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^space",term.labels,value=T)

  if( length(the_call) == 0 ) {
    return(list(covariance = "exponential",
                nu = 0.5))
  } else {}

  the_call<- gsub("space","~space",the_call)
  new_terms<- terms(formula(the_call),specials="space")
  attr(new_terms,"intercept")<- 0
  x<- model.frame(new_terms,data=data)[[1]]
  x<- as.list(x)
  names(x)<- c("covariance","nu")
  x$nu<- as.numeric(x$nu)

  return(x)
}





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
                      "lognormal", # 5
                      "binomial", # 6
                      "atLeastOneBinomial") # 7
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
               "matern", # 2
               "matern32") # 3
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

#' Retrieve observation mean covariate data from a formula and a data.frame.
#'
#' @param x A formula object with terms grouped in a \code{mean(...)} function.
#' @param data A data.frame containing the covariate data.
#' @param return Either "model.matrix"  or "model.frame"
#'
#' @return The design matrix for the covariates.
.mean_design_from_formula<- function(x,data,return = "model.matrix") {
  the_terms<- terms(x,specials=c("mean","time","space"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^mean",term.labels,value=T)
  if( length(the_call) == 0 ) {
    the_df<- matrix(0,nrow=nrow(data),ncol=0)
    return(the_df)
  } else {}
  new_formula<- sub("mean\\(","~",the_call)
  new_formula<- sub("\\)$","",new_formula)
  new_terms<- terms(formula(new_formula))
  attr(new_terms,"intercept")<- 0
  the_df<- switch(return,
    model.matrix = model.matrix(new_terms,data=data),
    model.frame = model.frame(new_terms,data=data)
  )
  attr(the_df,"assign")<- NULL
  rownames(the_df)<- NULL
  return(the_df)
}





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

#' Retrieve response data from a formula and a data.frame.
#'
#' @param x A formula object with a non-empty left-hand side.
#' @param data A data.frame to retrieve the response data from.
#'
#' @return A vector containing the response variable, with a `name' attribute
#'  containing the name of the response variable.
.response_from_formula<- function(x,data) {
  the_terms<- terms(x,specials=c("mean","time","space"))
  if( attr(the_terms,"response") == 0 ) {
    stop("Response variable must be specified.")
  } else {}
  idx<- attr(the_terms,"response")
  var_call<- attr(the_terms,"variables")[[idx+1]] # +1 because [[1]] is just `list`
  response<- with(data,eval(var_call))
  attr(response,"name")<- deparse(var_call)
  return(response)
}





# S

#' Retrieve a sample size from a formula and a data.frame.
#'
#' @param x A formula object with a sample.size(...) function containing one entry.
#' @param data A data.frame containing the sample size information.
#' @param nullReturn If there is no sample.size(...) function, should a vector of ones be returned?
#'
#' @return A data.frame with a single column for the sample size.
.sample_size_from_formula<- function(x,data,nullReturn=F) {
  the_terms<- terms(x,specials=c("sample.size","mean","time","space"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^sample.size",term.labels,value=T)
  if( length(the_call) == 0 ) {
    if( nullReturn == F ) {
      the_df<- matrix(0,nrow=nrow(data),ncol=0)
    } else if( nullReturn == T ) {
      the_df<- matrix(1,nrow=nrow(data),ncol=1)
    }
    return(the_df)
  } else {}
  new_formula<- sub("sample.size\\(","~",the_call)
  new_formula<- sub("\\)$","",new_formula)
  new_terms<- terms(formula(new_formula))
  attr(new_terms,"intercept")<- 0
  the_df<- model.frame(new_terms,data=data)

  attr(the_df,"assign")<- NULL
  rownames(the_df)<- NULL
  return(the_df)
}





# T

#' Retrieve a time index from a formula and a data.frame.
#'
#' @param x A formula object with a time(...,type) function containing one entry,
#'  and a `type' argument which can be one of "ar1", "rw", or "independent".
#' @param data A data.frame containing the time information.
#'
#' @return A vector containing the index, with a `name' attribute
#'  containing the name of the time index and a `type' attribute containing the
#'  type of time process (ar1, random walk, or independent).
.time_from_formula<- function(x,data) {
  time<- function(x,type="ar1") {
    type<- pmatch(type[[1]],c("ar1","rw","independent"))
    type<- c("ar1","rw","independent")[type]
    attr(x,"type")<- type
    return(x)
  }
  the_terms<- terms(x,specials=c("mean","time","space"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^time",term.labels,value=T)
  if( length(the_call) == 0 ) {
    x<- rep(0,nrow(data))
    attr(x,"name")<- "Time"
    attr(x,"type")<- "independent"
    return(x)
  } else {}
  new_formula<- sub("time","~",the_call)
  new_formula<- sub(",.*","",new_formula)
  new_formula<- sub("\\(","",new_formula)
  new_formula<- sub("\\)$","",new_formula)
  new_terms<- terms(formula(new_formula))
  if( length(attr(new_terms,"term.labels")) > 1 ) {
    stop("Only one variable is allowed for time.")
  } else {
    the_name<- attr(new_terms,"term.labels")[[1]]
  }
  new_formula<- sub("time","~time",the_call)
  new_terms<- terms(formula(new_formula),specials="time")
  attr(new_terms,"intercept")<- 0
  x<- model.frame(new_terms,data=data)[[1]]
  attr(x,"name")<- the_name

  return(x)
}





# U





# V





# W





# X





# Y





# Z
