% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{strv_prepare}
\alias{strv_prepare}
\alias{strv_prepare_process}
\alias{strv_prepare_observations}
\title{Create an object of class \code{starve}.}
\usage{
strv_prepare(
  formula,
  data,
  nodes = data,
  n_neighbours = 10,
  persistent_graph = NA,
  transient_graph = NA,
  distribution = "gaussian",
  link = "default",
  silent = TRUE,
  max_dist = Inf,
  distance_units = "km",
  fit = FALSE,
  ...
)

strv_prepare_process(data, nodes, persistent_graph, settings)

strv_prepare_observations(data, process, settings, distribution, link)
}
\arguments{
\item{formula}{A formula object. See the 'Details' section below.}

\item{data}{An `sf` object containing point geometries, and any other
variables needed to fit the model.}

\item{nodes}{An `sf` object containing point geometries, defaulting to
\code{data}. These locations will be used as the locations for the
persistent graph. Can also be an "inla.mesh" object, in which case
it defines both the persistent graph locations and the persistent graph
itself.}

\item{n_neighbours}{An integer (default=10) giving the (maximum) number of
parents for each node.}

\item{persistent_graph}{If an object of class \code{dag} is supplied, that
graph is used for the persistent graph.}

\item{transient_graph}{If an object of class \code{dag} is supplied, that
graph is used for the transient graph.}

\item{distribution}{A character vector giving the response distribution(s).
See \code{get_starve_distributions("distribution")} for valid options.
Defaults to "gaussian".}

\item{link}{A character vector giving the response link function(s). See
\code{get_starve_distributions("link")} for valid options.
The default link function changes depending on the response distribution.}

\item{silent}{Logical. Should intermediate calculations be printed?}

\item{max_dist}{Numeric. The maximum allowable distance for edges in the
transient graph, or for graphs computed when using the
\code{starve_predict} function. Unless this has a units attribute, units
are assumed to be the same as the supplied \code{distance_units}.}

\item{distance_units}{Any value that can be used as a \code{units} object
from the \code{units} package. Which distance units should the model use?
Defaults to "km".}

\item{fit}{Logical (Default=FALSE). Should parameter estimates be found?
If so, the starting values for the optimizer will use the default values.}

\item{...}{Extra options to pass to \link{strv_fit} if fit=TRUE}

\item{settings}{A settings object}

\item{process}{A process object.}
}
\value{
A starve object. If fit=TRUE, the returned model parameters will be
  estimated using the \link{strv_fit} function using the default starting
  values.
}
\description{
'strv_prepare' is used to take an existing `simple features` data.frame
  with point geometries, time information, covariates, and a response
  variable and perform all of the pre-processing steps necessary to fit a
  model with the \code{fit} function. See the description for
  \link{strv_prepare_process} and \link{strv_prepare_observations} for more
  details on how each part is prepared.
}
\details{
The formula object should always be of the form \code{y ~ sample.size(n) +
  mean(x + z) + time(t, type = "ar1") + space("matern", nu = 1.5)},
  though possibly with some terms missing.

The variable y should be replaced with the desired response variable.

The sample.size(...) term is only used if the response distribution is
  \code{binomial}, \code{atLeastOneBinomial}, or \code{tweedie}.
  If it is missing the sample sizes are assumed to all be 1.

The variables in the \code{mean(...)} term are used as covariates for the
  mean of the response variable. Any formula valid for the \code{lm} command
  can be used inside the \code{mean(...)}, such as \code{I(x^2)}. Any missing
  covariate values will likely cause errors. If the \code{mean(...)} term is
  missing, no covariates will be used.

The \code{time(...)} term indicates which column, if any, holds the time
  index. The variable t should be replaced with the desired time index. There
  are currently three valid options for the `type' argument in
  \code{time(t,type="ar1")} -- "ar1" for an AR(1) structure, "rw" for a
  random walk, and "independent" for independent spatial fields each year. If
  the \code{time(...)} term is missing, all observations are assumed to be
  at the same time and a purely spatial model is used.

The \code{space(...)} term specifies the spatial covariance function. See
  \code{get_starve_distributions("covariance")} for valid names to supply.
  If using the "matern" option you can supply a value for the smoothness
  parameter nu, which will be held constant in model fitting. If nu is not
  given, then it will be freely estimated in the model. If the
  \code{space(...)} term as a whole is missing, an exponential covariance
  function is assumed.
}
\section{Functions}{
\itemize{
\item \code{strv_prepare_process()}: Creates a new process object with the correct
  dimensions for the temporal random effects, persistent graph random
  effects, and transient graph random effects. Initializes the temporal and
  spatial parameters for the model according to the options specified in the
  formula element of the settings argument. Constructs the persistent and
  transient graph, see \link{construct_graph}.

  Before creating the persistent graph any duplicate locations in nodes
  are removed. Before creating the transient graph any location in data that
  is present in nodes is removed.

\item \code{strv_prepare_observations()}: Creates a new observation object with the correct dimensions
for the random effect predictions. Initializes the response distribution
and fixed effect parameters for the model according to the options specified
in the formula element of the settings argument. Also adds a column "graph_idx"
to the supplied data.

}}
\seealso{
starve_class
}
