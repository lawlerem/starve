#' @include classes.R getset.R generics.R utility.R
NULL

#####################
###               ###
### Construct_dag ###
###               ###
#####################

#' @param edges A list of edges
#' @param distances A list of distances
#' @param distance_units Which distance units to use
#'
#' @noRd
setMethod(
    f = "initialize",
    signature = "dag",
    definition = function(
      .Object,
      edges = list(),
      distances = list(),
      distance_units = "km") {
  edges(.Object)<- edges
  distances(.Object)<- distances
  distance_units(.Object)<- distance_units

  return(.Object)
})


##################
###            ###
### Access_dag ###
###            ###
##################

#' @param x An object
#'
#' @export
#' @describeIn dag_class Get edge list
setMethod(
    f = "edges",
    signature = "dag",
    definition = function(x) {
  return(x@edges)
})

# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn dag_class Set edge list
#' @noRd
setReplaceMethod(
    f = "edges",
    signature = "dag",
    definition = function(x, value) {
  x@edges<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn dag_class Get list of edge distances
setMethod(
    f = "distances",
    signature = "dag",
    definition = function(x) {
  return(x@distances)
})
# #' @param x An object
# #' @param value A replacement value
# #'
# #' @describeIn dag_class Set list of edge distances
#' @noRd
setReplaceMethod(
    f = "distances",
    signature = "dag",
    definition = function(x, value) {
  x@distances<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn dag_class Get distance units
setMethod(
    f = "distance_units",
    signature = "dag",
    definition = function(x) {
  return(x@distance_units)
})
#' @param x An object
#' @param value A replacement value
#' @export
#' @describeIn dag_class Set distance units. Edge distances are automatically
#'   converted to the new units.
setReplaceMethod(
    f = "distance_units",
    signature = "dag",
    definition = function(x, value) {
  if( length(distance_units(x)) > 0 ) {
    # Only convert units if units were previously set
    dists<- distances(x)
    dists<- lapply(dists, function(mat) {
      mat<- units::set_units(mat, distance_units(x), mode = "standard")
      mat<- units::set_units(mat, value, mode = "standard")
      mat<- units::set_units(mat, NULL)
      return(mat)
    })
    distances(x)<- dists
  } else {}
  x@distance_units<- value
  return(x)
})



###############
###         ###
### Utility ###
###         ###
###############


#' Construct directed acyclic graphs
#'
#' @param x,y \code{sf} objects of point geometries
#' @param settings An object of class \code{settings}
#' @param silent Should intermediate calculations be shown?
#'
#' @return An object of class \code{dag}; construct_dag instead returns a list
#'   with elements "locations" giving the sorted copy of locations and "dag"
#'   giving the constructed graph of class \code{dag}.
#'
#' @seealso dag_class For class definition of directed acyclic graphs
#'
#' @name construct_graph
NULL



#' @describeIn construct_graph Construct a directed acyclic graph for a single
#'   \code{sf} object. This function uses a greedy algorithm to sort the
#'   locations of x. The first location of x is the first location of the sorted
#'   copy. Subsequent locations of the sorted copy are found iteratively by
#'   takingthe as-of-yet unsorted location closest to any of the already sorted
#'   locations.
#'
#'   After sorting the locations, the directed acyclic graph is constructed by
#'   first creating an initial graph element with no "from" vertices and "to"
#'   vertices consisting of the first n sorted locations (where n equals the
#'   \code{n_neighbours} element of the settings argument). Any locations not
#'   present in the "to" vertices of the initial graph elements are given their
#'   own graph element with a single "to" vertex for that location and "from"
#'   vertices the n closest (by distance) locations before it in the sorted
#'   copy.
#'
#'   A vertex with index i in either the "to" or "from" vertices represents the
#'   location in row i of the sorted copy of x.
construct_persistent_graph<- function(
    x,
    settings = new("settings"),
    silent = TRUE) {
  dist_matrix<- as.matrix(
    units::set_units(
      sf::st_distance(x),
      distance_units(settings),
      mode = "standard"
    )
  )
  dag<- dist_to_dag(
    d = dist_matrix,
    n_neighbours = min(nrow(x), n_neighbours(settings))
  )
  x<- x[dag$order + 1, ]
  dag<- new(
    "dag",
    edges = dag$edge_list,
    distances = dag$dist_list,
    distance_units = distance_units(settings)
  )
  dag<- convert_idxC_to_R(dag)
  return(list(
    locations = x,
    dag = dag
  ))
}



#' @param time A numeric vector containing the time index of each row in x.
#'
#' @describeIn construct_graph Construct a directed acyclic graph with "to"
#'   vertices representing locations in x and "from" vertices representing
#'   locations in y. Unlike strv_construct_persistent_graph, neither x nor y are
#'   sorted, but the time argument must already be sorted when given as an
#'   argument (make sure that the locations in x are likewise sorted according7
#'   to their respective times).
#'
#'   The locations in x are first split into groups according to the values of
#'   thetime argument. Then within each group each location is given a graph
#'   element with a single "to" vertex for that location (in x) and whose "from"
#'   vertices are the n closest locations (in y) (where n equals the
#'   \code{n_neighbours} element of the settings argument).
#'
#'   A vertex with index i in the "to" vertices represents the location in row i
#'   in the subset of x in the same time index group. Note that the same value
#'   of i may be used if there is more than one unique value in the time
#'   argument. A vertex with i in the "from" vertices represents the location in
#'   row i of y.
construct_transient_graph<- function(
    x,
    y,
    time = 0,
    settings = new("settings"),
    silent = TRUE) {
  if( nrow(x) == 0 ) {
    return(new("dag", distance_units = distance_units(settings)))
  } else {}

  # Make sure geometry columns are named the same
  colnames(y)[colnames(y) == attr(y,"sf_column")]<- attr(x, "sf_column")
  sf::st_geometry(y)<- attr(x, "sf_column")

  # Parents won't be eligible if their distance is too far
  max_dist<- units::set_units(
    max_distance(settings),
    distance_units(settings),
    mode = "standard"
  )
  max_distance(settings)<- as.numeric(
    units::set_units(
      max_dist,
      "m",
      mode = "standard"
    )
  )
  ### st_nn expects meters.

  if( length(time) == 1 ) {
    time<- rep(0, nrow(x))
  } else if( length(time) != nrow(x) ) {
    stop("Time needs to be the same length as x.")
  } else if( !all.equal(c(time), sort(time)) ) {
    stop("Time must be sorted.")
  } else {}
  to_vals<- do.call(
    c,
    lapply(
      rle(c(time))$lengths,
      seq
    )
  )

  # Find distances from tg locs to pg locs
  dist_matrix<- as.matrix(
    units::set_units(
      sf::st_distance(x, y),
      distance_units(settings),
      mode = "standard"
    )
  )
  pg_dist_matrix<- as.matrix(
    units::set_units(
      sf::st_distance(y),
      distance_units(settings),
      mode = "standard"
    )
  )

  dag<- dist_to_tg_dag(
    dist_matrix,
    pg_dist_matrix,
    n_neighbours(settings)
  )

  # Reset "to" counter at start of new years
  for( i in seq_along(dag$edge_list) ) {
    dag$edge_list[[i]][["to"]]<- to_vals[[i]] - 1
  }
  dag<- new(
    "dag",
    edges = dag$edge_list,
    distances = dag$dist_list,
    distance_units = distance_units(settings)
  )
  dag<- convert_idxC_to_R(dag)

  return(dag)
}



#' @param pred A long_stars object
#' @param model A starve object
#'
#' @describeIn construct_graph Works essentially the same as
#'   \code{construct_transient_graph}, with a few minor differences. Instead of
#'   directly supplying the locations for the "from" vertices, the "from"
#'   vertices are taken from the locations used in the persistent graph and
#'   transient graph from the model argument.
#'
#'   The locations in pred are grouped according to their time index in pred.
#'   Each location is given a graph element with a single "to" vertex
#'   representing that location in pred. If that location is the same as any
#'   model location then the graph element has a single "from" vertex
#'   representing that model location, otherwise "from" vertices are the n
#'   closest model locations (where n equals the \code{n_neighbours} element of
#'   the settings argument).
#'
#'   A vertex with index i in the "to" vertices represents the location in row i
#'   of pred (regardless of time index group). A vertex with index j in the
#'   "from" vertices represents the j'th location of the persistent graph if
#'   j <= k where k is the number of persistent graph locations, or represents
#'   the  (j-k)'th location of the transient graph with the same time index as
#'   the "to" vertex.
construct_prediction_graph<- function(
    pred,
    model,
    silent = TRUE) {
  settings<- settings(model)

  # Make sure geometry columns are named the same
  pred_sf_idx<- colnames(locations(pred)) == attr(locations(pred), "sf_column")
  model_sf_name<- attr(locations_from_stars(pg_re(model)), "sf_column")
  colnames(locations(pred))[pred_sf_idx]<- model_sf_name
  sf::st_geometry(locations(pred))<- model_sf_name

  # Parents won't be eligible if their distance is too far
  max_dist<- units::set_units(
    max_distance(settings),
    distance_units(settings),
    mode = "standard"
  )
  max_distance(settings)<- as.numeric(units::set_units(max_dist, "m"))
  ### st_nn expects meters.

  t<- time_from_formula(formula(model), locations(pred))
  if( !all.equal(c(t), sort(t)) ) {
    stop("The rows of pred must be sorted by their time index.")
  } else {}

  # Find distances from pred locs to graph locs
  pg_s<- sf::st_geometry(locations_from_stars(pg_re(model)))
  tg_s<- sf::st_geometry(locations(tg_re(model)))
  graph_locs<- sf::st_sf(
    t = c(
      rep(NA, length(pg_s)),
      time_from_formula(formula(model), locations(tg_re(model)), "vector")
    ),
    geom = c(pg_s, tg_s)
  )
  colnames(graph_locs)[2]<- model_sf_name
  sf::st_geometry(graph_locs)<- model_sf_name

  dist_matrix<- as.matrix(
    units::set_units(
      sf::st_distance(locations(pred), graph_locs),
      distance_units(settings),
      mode = "standard"
    )
  )
  graph_dist_matrix<- as.matrix(
    units::set_units(
      sf::st_distance(graph_locs),
      distance_units(settings),
      mode = "standard"
    )
  )

  # Set distances from pred loc to tg loc to INF if not at same time
  t_equal<- outer(
    c(time_from_formula(formula(model), locations(pred), "vector")),
    graph_locs$t,
    `==`
  )
  dist_matrix[!t_equal]<- Inf
  # Create a matrix that tells us how to convert an index from
  #   c(pg_locs, tg_locs) to an index from c(pg_locs, tg_locs[[t]])
  node_alignment<- t_equal
  node_alignment[is.na(node_alignment)]<- TRUE
  node_alignment[!node_alignment]<- NA

  seq_by_row<- lapply(
    rowSums(node_alignment, na.rm = TRUE),
    seq
  )
  node_alignment_t<- t(node_alignment)
  node_alignment_t[which(node_alignment_t)]<- do.call(c, seq_by_row)
  node_alignment<- t(node_alignment_t)
  # e.g. node_alignment[2, 5] = 4 means that for pred_loc 2, the 5th node of
  #  c(pg_locs, tg_locs) is the 4th node in this year's graph

  dag<- dist_to_pred_dag(
    dist_matrix,
    graph_dist_matrix,
    node_alignment - 1,
    n_neighbours(settings)
  )


  # Replace graph for pred locs that intersect graph locs
  intersects<- st_equals(locations(pred), graph_locs)
  intersects_idx<- which(lengths(intersects) == 1)
  intersects_list<- lapply(seq_along(intersects), function(i) {
    return(list(
      to = i,
      from = intersects[[i]] - 1
    ))
  })
  dag$edge_list[intersects_idx]<- intersects_list[intersects_idx]
  dag$dist_list[intersects_idx]<- lapply(intersects_idx,function(x) matrix(0))

  # Don't want to reset "to" counter at start of new years

  dag<- new(
    "dag",
    edges = dag$edge_list,
    distances = dag$dist_list,
    distance_units = distance_units(settings)
  )
  dag<- convert_idxC_to_R(dag)

  return(dag)
}






# #' @describeIn idx_exchange Add 1 to all vertices in the graph edge list
#' @noRd
setMethod(
    f = "convert_idxC_to_R",
    signature = "dag",
    definition = function(x) {
  edges(x)<- lapply(edges(x), function(e) {
    e[["to"]]<- e[["to"]] + 1
    e[["from"]]<- e[["from"]] + 1
    return(e)
  })
  return(x)
})

# #' @describeIn idx_exchange Subtract 1 from all vertices in the graph edge
# #'   list.
#' @noRd
setMethod(
    f = "convert_idxR_to_C",
    signature = "dag",
    definition = function(x) {
  edges(x)<- lapply(edges(x), function(e) {
    e[["to"]]<- e[["to"]] - 1
    e[["from"]]<- e[["from"]] - 1
    return(e)
  })
  return(x)
})
