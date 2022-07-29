#' @include classes.R getset.R generics.R process.R observations.R settings.R tracing.R TMB_out.R starve.R
NULL


#' Create an object of class \code{starve}.
#'
#' 'strv_prepare' is used to take an existing `simple features` data.frame
#'    with point geometries, time information, covariates, and a response variable
#'    and perform all of the pre-processing steps necessary to fit a model with the
#'    \code{fit} function. See the description for \link{strv_prepare_process} and
#'    \link{strv_prepare_observations} for more details on how each part is prepared.
#'
#' The formula object should always be of the form
#'   \code{y ~ sample.size(n)+mean(x+z) + time(t,type="ar1") + space("matern",nu=1.5)},
#'   though possibly with some terms missing.
#'
#' The variable y should be replaced with the desired response variable.
#'
#' The sample.size(...) term is only used if the response distribution is
#'   \code{binomial}, \code{atLeastOneBinomial}, or \code{tweedie}.
#'   If it is missing the sample sizes are assumed to all be 1.
#'
#' The variables in the \code{mean(...)} term are used as covariates for the mean
#'   of the response variable. Any formula valid for the \code{lm} command can be used
#'   inside the \code{mean(...)}, such as \code{I(x^2)}. Any missing covariate values
#'   will likely cause errors. If the \code{mean(...)} term is missing, no covariates will be used.
#'
#' The \code{time(...)} term indicates which column, if any, holds the time index.
#'   The variable t should be replaced with the desired time index. There are currently
#'   three valid options for the `type' argument in \code{time(t,type="ar1")} --
#'   "ar1" for an AR(1) structure, "rw" for a random walk, and "independent" for
#'   independent spatial fields each year. If the \code{time(...)} term is missing,
#'   all observations are assumed to be at the same time and a purely spatial model
#'   is used.
#'
#' The \code{space(...)} term specifies the spatial covariance function. See
#'   \code{get_starve_distributions("covariance")} for valid names to supply.
#'   If using the "matern" option you can supply a value for the smoothness
#'   parameter nu, which will be held constant in model fitting. If nu is not given,
#'   then it will be freely estimated in the model. If the \code{space(...)} term
#'   as a whole is missing, an exponential covariance function is assumed.
#'
#' @param formula A formula object. See the 'Details' section below.
#' @param data An `sf` object containing point geometries, and any other
#'   variables needed to fit the model.
#' @param nodes An `sf` object containing point geometries, defaulting to \code{data}.
#'  These locations will be used as the locations for the persistent graph.
#' @param n_neighbours An integer (default=15) giving the (maximum) number of parents for each node.
#' @param persistent_graph If an object of class \code{dag} is supplied, that
#'   graph is used for the persistent graph.
#' @param transient_graph If an object of class \code{dag} is supplied, that
#'   graph is used for the transient graph.
#' @param distribution A character vector giving the response distribution(s).
#'   See \code{get_starve_distributions("distribution")} for valid options.
#'   Defaults to "gaussian".
#' @param link A character vector giving the response link function(s). See
#'   \code{get_starve_distributions("link")} for valid options.
#'   The default link function changes depending on the response distribution.
#' @param silent Logical. Should intermediate calculations be printed?
#' @param max_dist Numeric. The maximum allowable distance for edges in the transient
#'   graph, or for graphs computed when using the \code{starve_predict} function.
#'   Unless this has a units attribute, units are assumed to be the same as
#'   the supplied \code{distance_units}.
#' @param distance_units Any value that can be used as a \code{units} object
#'   from the \code{units} package. Which distance units should the model use?
#'   Defaults to "km".
#' @param fit Logical. Should parameter estimates be found? If so, the starting
#'   values for the optimizer will use the default values.
#' @param ... Extra options to pass to \link{strv_fit} if fit=TRUE
#'
#' @return A starve object. If fit=TRUE, the returned model parameters will be
#'   estimated using the \link{strv_fit} function using the default starting values.
#'
#' @seealso starve_class
#'
#' @export
strv_prepare<- function(formula,
                        data,
                        nodes = data,
                        n_neighbours = 15,
                        persistent_graph = NA,
                        transient_graph = NA,
                        distribution = "gaussian",
                        link = "default",
                        silent = TRUE,
                        max_dist = Inf,
                        distance_units = "km",
                        fit = FALSE,
                        ...) {
  model<- new("starve")

  # Set the settings in the model
  settings(model)<- new("settings",
    formula = formula,
    n_neighbours = n_neighbours,
    distance_units = distance_units,
    max_distance = max_dist
  )

  # Set up the process
  process(model)<- strv_prepare_process(
    data = data,
    nodes = nodes,
    persistent_graph = persistent_graph,
    settings = settings(model)
  )

  # Set up the observations
  observations(model)<- strv_prepare_observations(
    data = data,
    process = process(model),
    settings = settings(model),
    distribution = distribution,
    link = link
  )

  if( fit ) {
    model<- strv_fit(model,silent = silent,...)
  } else {}

  return(model)
}



#' @param settings A settings object
#'
#' @describeIn strv_prepare Creates a new process object with the correct dimensions for the
#'   temporal random effects, persistent graph random effects, and transient
#'   graph random effects. Initializes the temporal and spatial parameters for
#'   the model according to the options specified in the formula element of the
#'   settings argument. Constructs the persistent and transient graph, see \link{construct_graph}.
#'
#'   Before creating the persistent graph any duplicate locations in nodes
#'   are removed. Before creating the transient graph any location in data that
#'   is present in nodes is removed.
strv_prepare_process<- function(data,
                                nodes,
                                persistent_graph,
                                settings ) {
  process<- new("process")

  # Return a time column with name and type (ar1/rw/etc) attributes
  time_col<- time_from_formula(formula(settings),data)
  time_seq<- seq(min(time_col),max(time_col))

  # time_effects = "data.frame"
  time_effects(process)<- stars::st_as_stars(
    list(w = array(0,dim=c(length(time_seq),n_response(formula(settings)))),
         se = array(0,dim=c(length(time_seq),n_response(formula(settings))))
    ),
    dimensions = stars::st_dimensions(time = time_seq,variable = response_names(formula(settings)))
  )
  names(stars::st_dimensions(time_effects(process)))[[1]]<- time_name(settings)


  # pg_re = "sf",
  uniq_nodes<- unique(nodes[,attr(nodes,"sf_column")])

  if( !identical(persistent_graph,NA) ) {
    distance_units(persistent_graph)<- distance_units(settings)
    persistent_graph(process)<- persistent_graph
  } else {
    graph<- construct_persistent_graph(uniq_nodes,settings=settings,silent=TRUE)
    uniq_nodes<- graph$locations
    persistent_graph(process)<- graph$dag
  }

  pg_re(process)<- stars::st_as_stars(
    list(w = array(0,dim=c(nrow(uniq_nodes),length(time_seq),n_response(formula(settings)))),
         se = array(0,dim=c(nrow(uniq_nodes),length(time_seq),n_response(formula(settings))))
    ),
    dimensions = stars::st_dimensions(geom = sf::st_geometry(uniq_nodes),
                                      time = time_seq,
                                      variable = response_names(formula(settings)))
  )
  names(stars::st_dimensions(pg_re(process)))[[2]]<- time_name(settings)



  # tg_re = "sf"
  # Construct transient graph if not supplied
  pg_locs<- locations_from_stars(pg_re(process))
  colnames(pg_locs)[colnames(pg_locs) == attr(pg_locs,"sf_column")]<- attr(data,"sf_column")
  st_geometry(pg_locs)<- attr(data,"sf_column")
  data<- data[lengths(st_equals(data,pg_locs)) == 0,]

  transient_graph(process)<- construct_transient_graph(
    x = data,
    y = pg_locs,
    time = time_from_formula(formula(settings),data),
    settings = settings
  )
  tg_re(process)<- new("long_stars",
                       locations = sf::st_sf(data.frame(
                          time_from_formula(formula(settings),data,return="column")
                         ),
                         geom = sf::st_geometry(data)
                       ),
                       var_names = response_names(formula(settings)))
  names(values(tg_re(process)))[[2]]<- "se"
  values(tg_re(process))$linear<- NULL
  values(tg_re(process))$linear_se<- NULL
  values(tg_re(process))$response<- NULL
  values(tg_re(process))$response_se<- NULL



  # parameters = "process_parameters"
  parameters<- new("process_parameters")
  # Returns name of covariance function and value of nu
  covariance<- covariance_from_formula(formula(settings))

  covariance_function(parameters)<- covariance$covariance
  # covariance_function<- takes care of settings spatial parameters,
  # but if nu is supplied for matern need to set nu
  for( i in seq_along(covariance$nu) ) {
    if( covariance$covariance[[i]] == "matern" & !is.na(covariance$nu[[i]]) ) {
      spatial_parameters(parameters)[[i]]["nu","par"]<- covariance$nu[[i]]
      spatial_parameters(parameters)[[i]]["nu","fixed"]<- TRUE
    } else {}
  }
  names(spatial_parameters(parameters))<- response_names(formula(settings))

  time_parameters(parameters)<- lapply(attr(time_col,"type"),function(tt) {
    df<- data.frame(
      par = c(0,
              switch(tt,
                     ar1 = 0,
                     independent = 0,
                     rw = 1),
              0),
      se = NA,
      fixed = c(FALSE,
                switch(tt,
                       ar1 = FALSE,
                       independent = TRUE,
                       rw = TRUE),
                FALSE),
      row.names = c("mu","ar1","sd")
    )
    if( length(unique(time_seq)) == 1 ) {
      # If purely spatial data, we don't need time parameters
      df[c("ar1","sd"),"fixed"]<- c(TRUE,TRUE)
    } else {}
    return(df)
  })
  names(time_parameters(parameters))<- response_names(formula(settings))

  parameters(process)<- parameters

  return(process)
}




#' @param process A process object.
#'
#' @describeIn strv_prepare Creates a new observation object with the correct dimensions
#'   for the random effect predictions. Initializes the response distribution
#'   and fixed effect parameters for the model according to the options specified
#'   in the formula element of the settings argument. Also adds a column "graph_idx"
#'   to the supplied data.
strv_prepare_observations<- function(data,
                                     process,
                                     settings,
                                     distribution,
                                     link) {
  observations<- new("observations")

  # data = "sf"
  # Put in lexicographic ordering by time, then S->N / W->E
  data<- order_by_location(data,time = data[[time_name(settings)]])

  data_predictions(observations)<- new("long_stars",
    sf::st_sf(data.frame(
      mean_design_from_formula(formula(settings),data,return = "all.vars"),
      sample_size_from_formula(formula(settings),data,unique_vars = TRUE),
      response_from_formula(formula(settings),data),
      time_from_formula(formula(settings),data,"column"),
      graph_idx = create_graph_idx(data,process,settings),
      data[,attr(data,"sf_column")]
    )),
    var_names = response_names(formula(settings))
  )



  # parameters = "observation_parameters"
  parameters<- new("observation_parameters")

  # Match supplied response distribution to valid options
  # response_distribution<- also takes care of response parameters
  # and link function
  response_distribution(parameters)<- rep(distribution,length.out=n_response(formula(settings)))
  names(response_parameters(parameters))<- response_names(formula(settings))

  # Match supplied link function with valid options
  if( !identical(link,"default") ) {
    link_function(parameters)<- rep(link,length.out=n_response(formula(settings)))
  } else {}

  # Set up fixed effects according to covariates formula
  nff<- colnames(mean_design_from_formula(formula(settings),data))
  fixed_effects(parameters)<- lapply(response_names(formula(settings)),function(rr) {
    return(data.frame(
      par = numeric(length(nff)),
      se = rep(NA,length(nff)),
      fixed = rep(FALSE,length(nff)),
      row.names = nff
    ))
  })
  names(fixed_effects(parameters))<- response_names(formula(settings))
  parameters(observations)<- parameters

  return(observations)
}
