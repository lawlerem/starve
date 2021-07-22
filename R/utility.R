#' @include classes.R getset.R generics.R
NULL

###
### Miscellaneous functions which have potential to be widely-used
###   within the package.
### Should be organized in alphabetical order.
###



# A

#' Append random effects for hind- and fore-casting
#'
#' @param x A staRVe_model object
#' @param time What times are needed? Only need to supply min and max values
#'
#' @return A copy of x with extra temporal and spatio-temporal random effects
#'
#' @noRd
.add_random_effects_by_time<- function(x,times) {
  # Get original random effects; only need first time for locations
  random_effects<- random_effects(x)
  nodes<- split(
    random_effects[,attr(random_effects,"sf_column")],
    random_effects[,attr(random_effects,"time_column"),drop=T]
  )[[1]]
  model_times<- unique(random_effects[,attr(random_effects,"time_column"),drop=T])

  if( min(times) < min(model_times) ) {
    extra_times<- seq(min(times),min(model_times)-1)

    # Add temporal random effects prior to the start of the original random effects
    extra_time_effects<- data.frame(
      w = 0,
      se = NA,
      time = extra_times
    )
    colnames(extra_time_effects)[[3]]<- attr(random_effects,"time_column")
    time_effects(x)<- rbind(
      extra_time_effects,
      time_effects(x)
    )

    # Add spatio-temporal random effects prior to the start of the original random effects
    extra_effects<- do.call(rbind,lapply(extra_times,function(t) {
      # Random effects for each new time, just replicating
      # the locations from the original random effects
      df<- sf::st_sf(data.frame(w = 0,
                                se = NA,
                                time = t,
                                nodes))
      colnames(df)[[3]]<- attr(random_effects,"time_column")
      return(df)
    }))
    random_effects(x)<- rbind(
      extra_effects,
      random_effects(x)
    )
  } else {}

  if( max(times) > max(model_times) ) {
    extra_times<- seq(max(model_times)+1,max(times))

    # Add temporal random effects after the end of the original random effects
    extra_time_effects<- data.frame(
      w = 0,
      se = NA,
      time = extra_times
    )
    colnames(extra_time_effects)[[3]]<- attr(random_effects,"time_column")
    time_effects(x)<- rbind(
      time_effects(x),
      extra_time_effects
    )

    # Add spatio-temporal random effects after the end of the original random effects
    extra_effects<- do.call(rbind,lapply(extra_times,function(t) {
      # Random effects for each new time, just replicating
      # the locations from the original random effects
      df<- sf::st_sf(data.frame(w = 0,
                                se = NA,
                                time = t,
                                nodes))
      colnames(df)[[3]]<- attr(random_effects,"time_column")
      return(df)
    }))

    random_effects(x)<- rbind(
      random_effects(x),
      extra_effects
    )
  } else {}

  attr(random_effects(x),"time_column")<- attr(random_effects,"time_column")
  attr(time_effects(x),"time_column")<- attr(random_effects,"time_column")
  return(x)
}






# B

#' An easy-to-run example, useful for quickly checking things work
#'
#' Run by entering \code{staRVe:::.birdFit()}
#'
#' @return A list with outputs from prepare_staRVe_model(...,fit=T), staRVe_simulate,
#'   and staRVe_predict (with forecasts).
#'
#' @noRd
.birdFit<- function() {
  small_bird<- staRVe::bird_survey[staRVe::bird_survey$year %in% 1998:2000,]
  small_bird<- cbind(x=rnorm(nrow(small_bird)),
                     small_bird)
  fit<- prepare_staRVe_model(
    cnt~x+time(year),
    small_bird,
    distribution="poisson",
    link="log",
    fit=F
  )
  spatial_parameters(fit)["range","par"]<- 80
  spatial_parameters(fit)["range","fixed"]<- T
  fit<- staRVe_fit(fit,silent=T)
  sim<- staRVe_simulate(fit)
  pred_locs<- do.call(rbind,lapply(2000:2010, function(t) {
    sf::st_sf(data.frame(x=rnorm(1),
                         year=t,
                         small_bird[1,"geom"]))
  }))
  pred<- staRVe_predict(fit,pred_locs,covariates=pred_locs,time=2000:2010)
  return(list(fit=fit,sim=sim,pred=pred))
}






# C

#' Retrieve a spatial covariance function from a formula.
#'
#' @param x A formula object with a space(covariance,nu) term. The covariance
#'   argument must be one those listed in get_staRVe_distributions("covariance").
#'   The `nu' argument is only necessary when using the Matern covariance, and
#'   supplying it fixes the value of nu (the smoothness parameter).
#'
#' @return A list containing the name of the covariance function and the value of nu.
#'
#' @noRd
.covariance_from_formula<- function(x,data=data.frame(1)) {
  # Set up what the "space" term does in the formula
  space<- function(formula,covariance,nu) {
    # Find the closest match for covariance function
    if( missing(covariance) ) {covariance<- "exponential"}
    covar<- pmatch(covariance,get_staRVe_distributions("covariance"))
    covar<- get_staRVe_distributions("covariance")[covar]

    # Grab value for nu, only used for "matern"
    # If nu is supplied (for matern) it's going to be held constant
    if( missing(nu) ) {
      nu<- NaN
    }
    covariance<- cbind(covar,nu)
    colnames(covariance)<- c("covariance","nu")

    return(covariance)
  }

  # Get out just the "space" term from the formula (as a character string)
  the_terms<- terms(x,specials=c("time","space","sample.size"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^space",term.labels,value=T)

  # (if the "space" term is missing, default to exponential)
  if( length(the_call) == 0 ) {
    return(list(covariance = "exponential",
                nu = 0.5))
  } else {}

  # Make the "space(...)" term a formula object. The way we defined the
  # "space" special above, x gives a matrix with 1 row with a column
  # each for the covariance function (character) and nu. Then we convert x
  # to a list for convenience
  the_call<- gsub("space","~space",the_call)
  new_terms<- terms(formula(the_call),specials="space")
  x<- model.frame(new_terms,data=data)[[1]]
  x<- as.list(x)
  names(x)<- c("covariance","nu")
  x$nu<- as.numeric(x$nu)

  return(x)
}

#' Convert covariance function (character) to (int)
#'
#' @param covariance Name of the covariance function to use. Check
#'   get_staRVe_distributions("covariance")
#'
#' @return The integer code for the named covariance function (index from 0)
#'
#' @noRd
.covariance_to_code<- function(covariance) {
  covar_code<- charmatch(covariance,get_staRVe_distributions("covariance"))
  if( is.na(covar_code) || covar_code == 0 ) {
    stop("Supplied covariance function is not implemented, or matches multiple covariance functions.")
  } else {
    covar_code<- covar_code - 1 # Convert to cpp
  }
}






# D

#' Convert response distribution (character) to (int)
#'
#' @param distribution Name of the response_distribution to use. Check
#'   get_staRVe_distributions("distribution")
#'
#' @return The integer code for the named response distribution (index from 0)
#'
#' @noRd
.distribution_to_code<- function(distribution) {
  distribution_code<- charmatch(distribution,get_staRVe_distributions("distribution"))
  if( is.na(distribution_code) || distribution_code == 0  ) {
    stop("Supplied distribution is not implemented, or matches multiple distributions.")
  } else {
    distribution_code<- distribution_code - 1 # Convert to cpp
  }

  return(distribution_code)
}






# E





# F





# G

#' List of staRVe model options
#'
#' Print a list of implemented response distributions, link functions, and
#'   covariance functions.
#'
#' @section Response Distributions:
#' \describe{
#'   \item{gaussian - }{Linear predictor determines the mean,
#'     has a variance parameter.}
#'   \item{gamma - }{Used for model strictly positive continuous data such as biomass,
#'     linear predictor determines the mean, has a variance parameter.}
#'   \item{lognormal - }{Used for modelling strictly positive continuous data such
#'     as biomass, linear predictor determines the mean, has a variance parameter.}
#'   \item{poisson - }{Typically used for count data, linear
#'     predictor determines the intensity parameter.}
#'   \item{negative binomial - }{Typically used for over-dispersed count data,
#'     linear predictor determines the mean, has an overdispersion
#'     parameter (>1 results in overdispersion).}
#'   \item{compois - }{Used to model over- and under-dispersed count data, linear
#'     predictor determines the mean, has a dispersion parameter. A dispersion <1
#'     results in under-dispersion, while a dispersion >1 results in over-dispersion.}
#'   \item{bernoulli - }{Used to model yes/no or presence/absence data, linear
#'     predictor determines the probability of a yes.}
#'   \item{binomial - }{Used to model the # of yesses in k yes/no trials, linear
#'     predictor determines the probability of a yes in a single trial. The
#'     sample size k for each observation can be supplied in the model formula
#'     through the sample.size(...) term.}
#'   \item{atLeastOneBinomial - }{Used to model the probability of observing at
#'     least one yes in k yes/no trials, linear predictor determines the probability
#'     of a yes in a single trial. The sample size k for each observation can be
#'     supplied in the model formula through the sample.size(...) term.}
#' }
#'
#' @section Link Functions:
#' \describe{
#'   \item{identity}{}
#'   \item{log}{}
#'   \item{logit}{}
#' }
#'
#' @section Covariance Functions:
#' \describe{
#'   \item{exponential - }{Matern covariance with smoothness nu = 0.5. Results in
#'     a non-differentiable Gaussian random field.}
#'   \item{gaussian - }{Limiting function of the Matern covariance as smoothness nu
#'     approaches infinity. Results in smooth (infinitely differentiable) Gaussian
#'     random field. The variance parameter in this covariance function gives the
#'     marginal variance of the spatial field.}
#'   \item{matern - }{Matern covariance with arbitrary smoothness nu. Results in
#'     Gaussian random fields that are floor(nu) times differentiable. The smoothness
#'     parameter is hard to estimate, so fixing it to a set value is recommended.
#'     This option will run much slower than other specific values of the Matern
#'     covariance function (e.g. exponential).}
#'   \item{matern32 - }{Matern covariance with smoothness nu = 1.5. Results in
#'     a smooth (once differentiable) Gaussian random field.}
#' }
#'
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
  # All of these indices have to align with the C++ code
  if( "distribution" %in% which ) {
    # Check C++ in src/include/family.hpp
    distributions<- c("gaussian", # 0
                      "poisson", # 1
                      "negative binomial", # 2
                      "bernoulli", # 3
                      "gamma", # 4
                      "lognormal", # 5
                      "binomial", # 6
                      "atLeastOneBinomial", # 7
                      "compois") # 8
    names(distributions)<- rep("distribution",length(distributions))
  } else { distributions<- character(0) }

  if( "link" %in% which ) {
    # Check C++ in src/include/family.hpp
    links<- c("identity", # 0
              "log", # 1
              "logit") # 2
    names(links)<- rep("link",length(links))
  } else { links<- character(0) }

  if( "covariance" %in% which ) {
    # Check C++ in src/include/covariance.hpp
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

#' Convert an R-INLA mesh to a dag
#'
#' @param x An inla.mesh object. I.e., output from INLA::inla.mesh.2d
#' @param crs The coordinate system used. If missing, taken from x
#' @param n_neighbours How many vertices should be used to start the graph?
#'   Using at least 10 is recommended.
#'
#' @return A list containing node locations and a persistent graph.
#'
#' @noRd
.inla.mesh_to_dag<- function(
  x,
  crs = NA,
  n_neighbours) {
  locs<- as.data.frame(x$loc[,c(1,2)])
  m<- as.matrix(x$graph$vv)
  parents<- function(m,v) {
    all_parents<- do.call(c,lapply(v,function(j) {
      which( m[,j] == 1 )
    }))
    all_parents<- unique(all_parents[!(all_parents%in% v)])
    return(all_parents)
  }
  children<- function(m,v) {
    all_children<- do.call(c,lapply(v,function(i) {
      which( m[i,] == 1 )
    }))
    all_children<- unique(all_children[!(all_children %in% v)])
    return(all_children)
  }
  put_v_at_k<- function(m,v,k) {
    order<- c(v,setdiff(k:nrow(m),v))
    m[k:nrow(m),]<- m[order,]
    if( ncol(m) == nrow(m) ) { # Quick check for adjacency matrix
      m[,k:nrow(m)]<- m[,order]
    } else {}
      return(m)
  }

  ### Starting vertices
  start<- 1
  m0<- m; m0[lower.tri(m0)]<- 0
  joint<- 1
  cntr<- length(joint)
  while( length(joint) < n_neighbours ) {
    joint<- c(joint,children(m0,joint))
    joint<- c(joint,parents(m0,joint))
    if( length(joint) == cntr ) {
      warning(paste("Could not find",n_neighbours,"nodes to start."))
      break
    } else { }
    cntr<- length(joint)
  }
  joint<- head(joint,n_neighbours)
  m<- put_v_at_k(m,v=joint,k=start)
  locs<- put_v_at_k(locs,v=joint,k=start)
  start<- start+length(joint)

  while( start < nrow(m) ) {
    ### First child of 1:start with maximum # of parents
    m0<- m; m0[lower.tri(m0)]<- 0
    parent_length<- sapply(start:ncol(m), function(v) {
      p<- parents(m0,v)
      p<- intersect(p,1:(start-1))
      return(length(p))
    })
    v<- start-1+which.max(parent_length)
    m<- put_v_at_k(m,v=v,k=start)
    locs<- put_v_at_k(locs,v=v,k=start)
    start<- start+1
  }
  m0<- m; m0[lower.tri(m0)]<- 0

  locs<- sf::st_as_sf(locs,coords=c(1,2))
  if( !is.null(x$crs) ) {
    sf::st_crs(locs)<- x$crs
  }
  sf::st_crs(locs)<- crs

  # Make sure each vertex has at least n_neighbour parents
  edge_list<- vector(mode="list",length=nrow(m)-n_neighbours+1)
  edge_list[[1]]<- list(to = seq(n_neighbours),
                        from = numeric(0))

  edge_list[2:length(edge_list)]<- lapply((n_neighbours+1):nrow(m),function(i) {
    m1<- m0[1:i,1:i] # Make sure you don't use any nodes past node i, ensures acyclicality
    pars<- parents(m1,i)
    cntr<- length(pars)
    while( cntr < n_neighbours ) {
      pars<- unique(c(pars,parents(m1,c(pars,i)),children(m1,c(pars,i))))
      if( length(pars) == cntr ) {
        warning(paste("Could not find",n_neighbours,"parents for node",i))
        break
      } else {
        cntr<- length(pars)
      }
    }

    return(list(to = i,
                from = head(pars,n_neighbours)))
  })

  # Convert mesh adjacency matrix to weighted adjacency matrix
  if(!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package igraph needed to use inla.mesh for nodes. Please install it.",
      call. = FALSE)
  }

  dist_matrix<- matrix(Inf,nrow=nrow(m),ncol=ncol(m))
  for( i in 1:nrow(m) ) {
    for( j in 1:ncol(m) ) {
      if( m[i,j] == 1 ) {
        dist_matrix[i,j]<- sf::st_distance(locs[i,],locs[j,])
      } else {}
    }
  }
  dist_matrix<- apply(expand.grid(seq(nrow(m)),seq(ncol(m))),MARGIN=1,function(rc) {
    row<- rc[[1]]
    col<- rc[[2]]
    if( m[row,col] == 0 ) {
      ans<- Inf
    } else {
      ans<- sf::st_distance(locs[row,],locs[col,])[1,1]
    }
  })
  dist_matrix<- matrix(dist_matrix,nrow=nrow(m))
  igraph<- igraph::graph_from_adjacency_matrix(dist_matrix,weighted=T)
  dist_matrix<- igraph::distances(igraph)

  if( !requireNamespace("MASS",quietly=T) ) {
    stop("Package MASS needed to use inla.mesh for nodes. Please install it.",
      call. = FALSE)
  } else {}
  dist_list<- lapply(edge_list,function(edges) {
    all_v<- c(edges$to,edges$from)
    dists<- dist_matrix[all_v,all_v]
    dists<-  dist(MASS::isoMDS(dists,trace=F)$points,
                  diag = T,
                  upper = T)
    return(as.matrix(dists))
  })

  a_dist<- sf::st_distance(locs[1,],locs[2,])
  if( "units" %in% class(a_dist) ) {
    dist_matrix<- units::set_units(dist_matrix,
                                   units(a_dist),
                                   mode = "standard")
    distance_units<- as.character(units(a_dist))
  } else {
    warning("Could not infer distance units, assuming meters. Supply a coordinate reference system if available")
    distance_units<- "m"
    dist_matrix<- units::set_units(dist_matrix,
                                   "m",
                                   mode = "standard")
  }

  return(list(nodes = locs,
              persistent_graph = new("dag",
                edges = edge_list,
                distances = dist_list,
                distance_units = distance_units),
              adjacency_matrix = m,
              distance_matrix = dist_matrix))
}





# J





# K





# L

#' Convert link function (character) to (int)
#'
#' @param link Name of the link function to use. Check
#'   get_staRVe_distributions("link")
#'
#' @return The integer code for the named link function (index from 0)
#'
#' @noRd
.link_to_code<- function(link) {
  link_code<- charmatch(link,get_staRVe_distributions("link"))
  if( is.na(link_code) || link_code == 0 ) {
    stop("Supplied link function is not implemented, or matches multiple link functions.")
  } else {
    link_code<- link_code - 1 # Convert to cpp
  }

  return(link_code)
}

#' Convert a "map" list to use in TMB::MakeADFun
#'
#' Parameters that should be fixed have a value of TRUE, which is converted
#'   to NA (as a factor). NAs in the final list passed to TMB will be held constant
#'   in the model, while everything else is freely estimated.
#'
#' @param x A "logical" map list
#'
#' @return A copy of x where logical values are converted to factors
#'
#' @noRd
.logical_to_map<- function(x) {
  y<- seq_along(x)
  y[x]<- NA
  y<- as.factor(y)

  return(y)
}






# M

#' Retrieve observation mean covariate data from a formula and a data.frame.
#'
#' @param x A formula object.
#' @param data A data.frame containing the covariate data.
#' @param return Either "model.matrix", "model.frame", "all.vars"
#'
#' @return The design matrix for the covariates. If return is set to "model.matrix",
#'   factors, interaction terms, etc. are expanded to their own variables.
#'
#' @noRd
.mean_design_from_formula<- function(x,data,return = "model.matrix") {
  data<- as.data.frame(data)
  the_terms<- delete.response(terms(x,specials=c("time","space","sample.size")))
  if( nrow(attr(the_terms,"factors")[-unlist(attr(the_terms,"specials")),,drop=F]) == 0 ) {
    return(matrix(0,ncol=0,nrow=nrow(data)))
  } else {}
  new_terms<- drop.terms(the_terms,unlist(attr(the_terms,"specials")))
  the_df<- switch(return,
    # model.matrix expands factors into dummy variables, and expands
    # interaction terms, etc.
    model.matrix = model.matrix(new_terms,data=data),
    # model.frame returns just the covariates needed to eventually
    # create the model.matrix (no expansion). Does expand poly() (and other specials?)
    model.frame = model.frame(new_terms,data=data),
    # Returns the subset of the original the data.frame
    all.vars = data[,all.vars(formula(new_terms)),drop=F]
  )
  attr(the_df,"assign")<- NULL
  attr(the_df,"contrasts")<- NULL
  if( "(Intercept)" %in% colnames(the_df) ) {the_df<- the_df[,-grep("(Intercept)",colnames(the_df)),drop=F]}
  rownames(the_df)<- NULL
  return(the_df)
}

#' Retrieve spatial mean covariate data from a formula and a data.frame
#'
#' @param x A formula object
#' @param data A data.frame containing the covariate data
#' @param return Either "model.matrix", "model.frame", or "all.vars"
#'
#' @return The design matrix for the covariates designated inside the space(...)
#'   special.
#'
#' @noRd
.mean_design_from_space_formula<- function(x,data,return = "model.matrix") {
  # Pick out the formula inside the space special
  space<- function(x,covariance,nu) {
    if( missing(x) ) {
      return("~space()")
    }
    return(paste0("~",deparse(substitute(x)),"+space()"))
  }

  the_terms<- terms(x,specials=c("time","space","sample.size"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^space",term.labels,value=T)

  # if the "space" term is missing, return 0 column matrix
  if( length(the_call) == 0 ) {
    return(matrix(0,ncol=0,nrow=nrow(data)))
  } else {}

  the_call<- gsub("space","~space",the_call)
  new_terms<- terms(formula(the_call),specials="space")
  covar_formula<- formula(eval(attr(new_terms,"variables")[[2]]))
  return(.mean_design_from_formula(covar_formula,data,return))
}




# N

#' Check which covariates are used inside the "mean(...)" term of a formula
#'
#' @param x A formula object with terms grouped in a \code{mean(...)} function.
#'
#' @return A character vector giving the names of covariates used in a formula.
#'
#' @noRd
.names_from_formula<- function(x) {
  # Don't use colnames(.mean_design_from_formula(...,return="all.vars")) since
  # I don't want to supply a data.frame
  the_terms<- delete.response(terms(x,specials=c("time","space","sample.size")))
  if( nrow(attr(the_terms,"factors")[-unlist(attr(the_terms,"specials")),,drop=F]) == 0 ) {
    return(character(0))
  }
  new_terms<- drop.terms(the_terms,unlist(attr(the_terms,"specials")))
  var_names<- all.vars(new_terms)
  return(var_names)
}






# O

#' Sort an \code{sf} object with point geometry by location.
#'
#' The sorting is determined by lexicographic ordering, in order of the coordinates
#'  of the geometry object. The locations are put in increasing order according
#'  to the first coordinate, and then ties are put in order according to the second
#'  coordinate, and so on. See \code{\link{order}}. The "time" parameter can be
#'  supplied as the first sorting argument, and then sort by spatial coordinates.
#'
#' @param x An \code{sf} object containing point geometries.
#' @param time If present, a time index to be the primary (slowest running)
#'  sorting term. Should be the actual time indices, not the name of the time index.
#' @param return Should be one of ``sort" or ``order". If ``sort", a sorted copy
#'  of \code{x} is returned. If ``order", a vector of sorted indices is returned.
#'
#' @return Either a copy of \code{x} whose rows as sorted (\code{return='sort'});
#'  or an integer vector of sorted indices (\code{return='order'}).
#'
#' @noRd
.order_by_location<- function(x,time=NULL,return="sort") {
    # Get location coordinate in data.frame form (separate column for each coordinate)
    coords<- as.data.frame(sf::st_coordinates(x))
    # If time is supplied, add it as the first column
    if( !is.null(time) ) {coords<- cbind(time,coords)} else {}

    # Put things in (increasing) lexicographic order
    the_order<- do.call(order,as.list(coords))
    if( return == "order" ) {
        # Return the sorted indices
        return(the_order)
    } else if( return == "sort" ) {
        # Return the sorted data.frame
        x<- x[the_order,]
        return(x)
    } else {
        stop("Argument return must be one of `sort' of `order'")
    }
}





# P





# Q





# R

#' Retrieve response data from a formula and a data.frame.
#'
#' @param x A formula object with a non-empty left-hand side.
#' @param data A data.frame from which to retrieve the response data.
#'
#' @return A vector containing the response variable, with a `name' attribute
#'  containing the name of the response variable.
#'
#' @noRd
.response_from_formula<- function(x,data) {
  # Get out just the left-hand side of the formula
  the_terms<- terms(x,specials=c("time","space","sample.size"))
  if( attr(the_terms,"response") == 0 ) {
    stop("Response variable must be specified.")
  } else {}
  # Get out
  idx<- attr(the_terms,"response")
  var_call<- attr(the_terms,"variables")[[idx+1]] # +1 because [[1]] is just `list`,
  # then the terms are in order starting at 2
  response<- with(data,eval(var_call)) # Get response variable data
  attr(response,"name")<- deparse(var_call) # Get response variable name
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
#'
#' @noRd
.sample_size_from_formula<- function(x,data,nullReturn=F) {
  # Get out just the "sample.size" term from the formula (as a character string)
  the_terms<- terms(x,specials=c("time","space","sample.size"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^sample.size",term.labels,value=T)

  # (if the "sample.size" term is missing, either return an empty matrix of
  # or a matrix with a column of ones
  if( length(the_call) == 0 ) {
    if( nullReturn == F ) {
      the_df<- matrix(0,nrow=nrow(data),ncol=0)
    } else if( nullReturn == T ) {
      the_df<- matrix(1,nrow=nrow(data),ncol=1)
    }
    return(the_df)
  } else {}

  # Make the expression inside "sample.size" a standalone formula, and
  # use that formula to get the sample.size data
  new_formula<- sub("sample.size\\(","~",the_call)
  new_formula<- sub("\\)$","",new_formula)
  new_terms<- terms(formula(new_formula))
  the_df<- model.frame(new_terms,data=data)

  attr(the_df,"assign")<- NULL
  rownames(the_df)<- NULL
  return(the_df)
}

#' Reform a list of \code{Raster*} objects to an \code{sf} data.frame.
#'
#' Take a list of covariate raster and convert them to an sf data.frame
#'
#' Each element of the supplied list should be a \code{Raster*} object for a
#'   single covariate (e.g.,~x[[1]] is a \code{RasterBrick} exclusively for
#'   a temperature covariate). The layer names for each of these \code{Raster*}
#'   objects needs to be of the form \code{T####} where \code{####} gives the
#'   specific time index.
#'
#' @param x A list of \code{Raster*} objects.
#' @param time_name A string giving the name of the time variable.
#'
#' @return An \code{sf} data.frame.
#'
#' @noRd
.sf_from_raster_list<- function(x,time_name) {
    # sf_list will be a list of sf objects, each of which has a columns
    # for a single covariate, the time index, and the geometry
  sf_list<- lapply(x,function(raster_brick) {
    # The actions in this lapply are applied to each covariate separately
    layer_names<- names(raster_brick)
    layer_list<- lapply(layer_names,function(name) {
      # This lapply takes the covariates for a single time and converts the
      # raster layer to an sf object
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

  # Make sure the names of the columns are the names of the covariates
  sf_list<- lapply(names(x), function(name) {
    colnames(sf_list[[name]])[[1]]<- name
    return(sf_list[[name]])
  })

  # Get the times and locations needed
  unique_times<- lapply(sf_list, function(x) {
    return(unique(x[,time_name,drop=T]))
  })
  unique_times<- sort(unique(do.call(c,unique_times)))
  unique_geoms<- unique(sf_list[[1]][,attr(sf_list[[1]],"st_geometry")]) # This
  # assumes the initial rasters are the same.


  sf_output<- lapply(unique_times,function(time) {
    # Create an sf object with all the needed locations for a single time
    time_sf<- cbind(time,unique_geoms)
    # Add in the covariates with a spatial join
    var_columns<- lapply(sf_list,function(var) {
      var_time<- var[var[,time_name,drop=T]==time,] # Get covariates for this time
      var_time[,time_name]<- NULL # Get rid of time column
      suppressMessages(var_time<- sf::st_join(unique_geoms,var_time))
      var_time<- sf::st_drop_geometry(var_time) # Drop geometry so it isn't duplicated
      return(var_time)
    })
    # Combine all the covariates together (by column) and add back the geometry
    var_df<- do.call(cbind.data.frame,var_columns)
    time_sf<- sf::st_sf(data.frame(var_df,time_sf))
    return(time_sf)
  })
  # Combine all the different times together (by row)
  sf_output<- do.call(rbind,sf_output)
  colnames(sf_output)[colnames(sf_output)=="time"]<- time_name

  return(sf_output)
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
#'
#' @noRd
.time_from_formula<- function(x,data) {
  # Set up what the "time" term does in the formula
  time<- function(x,type="ar1") {
    # Find the closest match for type of temporal structure
    # Why is this not in get_staRVe_distributions? They're not really different structures?
    type<- pmatch(type[[1]],c("ar1","rw","independent"))
    type<- c("ar1","rw","independent")[type]
    attr(x,"type")<- type
    return(x)
  }

  # Get out just the "time" term from the formula (as a character string)
  the_terms<- terms(x,specials=c("time","space","sample.size"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^time",term.labels,value=T)

  # If the "time" term is missing, create a dummy Time variable where all
  # observations happen at the same time. Type = independent because there
  # is only one time
  if( length(the_call) == 0 ) {
    x<- rep(0,nrow(data))
    attr(x,"name")<- "Time"
    attr(x,"type")<- "independent"
    return(x)
  } else {}

  # Make the "time(...)" term a formula object. The way we defined the "time"
  # special above, x gives a matrix with 1 column containing the times that
  # each row in data was observed. The matrix as a "type" attribute giving
  # the name of the temporal structure.
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
  x<- model.frame(new_terms,data=data)[[1]]
  attr(x,"name")<- the_name # Store the name of the time variable

  return(x)
}






# U





# V





# W





# X





# Y





# Z
