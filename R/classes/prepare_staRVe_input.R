#' Convert an `sf' object to a form appropriate for the TMB::MakeADFun function.
#'
#' .
#'
#' The formula object should always be of the form \code{y ~ mean(x+z) + time(t,type="ar1")}.
#' The variable y should be replaced with the desired response variable, and t
#' should be replaced with the desired time index. There are currently three
#' valid options for the `type' argument in \code{time(t,type="ar1")} -- "ar1" for
#' an AR(1) structure, "rw" for a random walk, and "independent" for independent
#' spatial fields each year.
#'
#' The variables in the \code{mean(...)} ``special" are linear predictors for the mean
#' of the response variable. Any formula valid for the \code{lm} command can be used
#' inside the \code{mean(...)}, however any missing values will likely cause errors.
#'
#' @param formula A formula object used to describe the model. The details are given
#'  under `Details'.
#' @param data An object of class `sf` containing point geometries. Data
#'  used in the `formula' object will be found here.
#' @param Process An object of class `sf` containing point geometries.
#'  The default value uses the same locations as the observations. All locations
#'  will be used in every time index, so special care should be taken in choosing
#'  the number of points present. Any data fields (include time) will be discarded.
#' @param n_neighbours An integer giving the number of parents for each node.
#' @param p_far_neighbours What percent of neighbours should be randomly selected?
#' @param distribution A character vector giving the response distribution. The
#'  default is "gaussian". See \code{get_staRVe_distributions("distribution")}.
#' @param link A character vector giving the response link function. The default
#'  is "identity". See \code{get_staRVe_distributions("link")}.
#' @param silent Should intermediate calculations be printed?
#' @param max_dist The maximum distance used to search for parents.
#'  Unless this has a units attribute, units are assumed to be the same as
#'  the supplied \code{distance_units}. See \code{\link{construct_dag}}.
#' @param distance_units Which units should be used for distances?
#'  See \code{\link{construct_dag}}.
#' @param fit Should the model be fit in this call? If true, only returns
#'  the fitted model.
#'
#' @return A list with three components. The first is a list of data and parameters
#'  to pass directly to TMB::MakeADFun. The second is a list of \code{sf} objects
#'  storing minimal versions of Observation (== data) and Process. The third is a list of
#'  settings used in the model (n_neighbours, distance_units, etc.).
#'
#' @export
prepare_staRVe_input<- function(formula,
                                data,
                                nodes = data,
                                n_neighbours = 10,
                                p_far_neighbours = 0.2,
                                distribution = "gaussian",
                                link = "identity",
                                silent = T,
                                max_dist = Inf,
                                distance_units = "km",
                                fit = F,
                                ...) {
  model<- new("staRVe_model")
  settings(model)<- new("staRVe_settings",
    formula = formula,
    n_neighbours = n_neighbours,
    p_far_neighbours = p_far_neighbours,
    distance_units = distance_units,
    max_distance = max_dist
  )
  process(model)<- prepare_staRVe_process(
    nodes = nodes,
    time = data,
    settings = settings(model),
  )
  observations(model)<- prepare_staRVe_observation(

  )
}
