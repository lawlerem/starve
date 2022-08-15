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
  st_geometry(y)<- attr(x, "sf_column")

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

  splitx<- split(
    x,
    factor(
      time - min(time) + 1,
      levels = seq(max(time) - min(time) + 1)
    ),
    drop = FALSE
  )
  tg<- lapply(splitx, function(t_x) {
    if( nrow(t_x) == 0 ) {
      return( NULL )
    } else {}

    # Remove any locations of t_x already in y
    t_x<- t_x[lengths(sf::st_equals(t_x, y)) == 0, ]
    if( nrow(t_x) == 0 ) {
      return( NULL )
    } else {}

    # Get nearest neighbours
    nn_list<- suppressMessages(
      nngeo::st_nn(
        x = t_x,
        y = y,
        returnDist = FALSE,
        sparse = TRUE,
        progress = !silent,
        k = n_neighbours(settings),
        maxdist = max_distance(settings)
      )
    )
    edge_list<- lapply(seq(nrow(t_x)), function(i) {
      return(list(
        to = i,
        from = nn_list[[i]]
      ))
    })
    dist_list<- lapply(edge_list, function(e) {
      dists<- sf::st_distance(
        rbind(
          t_x[e$to, attr(x, "sf_column")],
          y[e$from, attr(y, "sf_column")]
        )
      )
      dists<- units::set_units(
        dists,
        distance_units(settings),
        mode = "standard"
      )
      return(units::drop_units(dists))
    })

    return(list(
      edges = edge_list,
      distances = dist_list
    ))
  })


  return(new(
    "dag",
    edges = unname(do.call(c, lapply(tg, `[[`, 1))),
    distances = unname(do.call(c, lapply(tg, `[[`, 2))),
    distance_units = distance_units(settings)
  ))
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

  pred_sf_idx<- colnames(locations(pred)) == attr(locations(pred), "sf_column")
  model_sf_column<- attr(locations_from_stars(pg_re(model)), "sf_column")
  colnames(locations(pred))[pred_sf_idx]<- model_sf_column
  st_geometry(locations(pred))<- model_sf_column

  # Parents won't be eligible if their distance is too far
  max_dist<- units::set_units(
    max_distance(settings),
    distance_units(settings),
    mode = "standard"
  )
  max_distance(settings)<- as.numeric(units::set_units(max_dist, "m"))
  ### st_nn expects meters.

  pg_s<- sf::st_geometry(locations_from_stars(pg_re(model)))
  tg_s<- sf::st_geometry(locations(tg_re(model)))

  t<- time_from_formula(formula(model), locations(pred))
  if( !all.equal(c(t), sort(t)) ) {
    stop("The rows of pred must be sorted by their time index.")
  } else {}

  time_dim_vals<- stars::st_get_dimension_values(pg_re(model), time_name(model))
  g_by_t<- lapply(time_dim_vals, function(t) {
    pr_t<- time_from_formula(formula(model), locations(pred))
    pr_s<- locations(pred)[pr_t == t, ]
    if( length(tg_s) > 0 ) {
      tg_re_t<- time_from_formula(formula(model), locations(tg_re(model)))
      g_s<- sf::st_sf(
        geom = c(
          pg_s,
          sf::st_sfc(unique(tg_s[tg_re_t == t]))
        )
      )
    } else {
      g_s<- sf::st_sf(geom = pg_s)
    }

    # Find prediction locations that coincide with graph locations
    edge_list<- st_equals(pr_s, g_s)
    edge_list<- lapply(seq_along(edge_list), function(i) {
      return(list(
        to = i,
        from = edge_list[[i]]
      ))
    })

    # Find nearest neighbours for remaining locations
    from_lengths<- do.call(
      c,
      lapply(edge_list, function(e) return(length(e$from)))
    )
    nn_idx<- which(from_lengths == 0)
    if( length(nn_idx) > 0 ) {
      suppressMessages({
        nn_list<- nngeo::st_nn(
          x = pr_s[nn_idx, ],
          y = g_s,
          returnDist = FALSE,
          sparse = TRUE,
          progress = !silent,
          k = n_neighbours(settings),
          maxdist = max_distance(settings)
        )
      })
      for( i in seq_along(nn_idx) ) {
        edge_list[[nn_idx[[i]]]]$from<- nn_list[[i]]
      }
    } else {}

    # Get distance matrices
    dist_list<- lapply(edge_list, function(e) {
      if( length(e$from) == 1 ) {
        return(matrix(0))
      } else {
        dists<- sf::st_distance(
          rbind(
            pr_s[e$to, attr(pr_s, "sf_column")],
            g_s[e$from, , attr(g_s, "sf_column")]
          )
        )
        dists<- units::set_units(
          dists,
          distance_units(settings),
          mode = "standard"
        )
        return(units::drop_units(dists))
      }
    })

    return(list(
      edge_list = edge_list,
      dist_dist = dist_list
    ))
  })

  # Don't want edge$to to reset every year
  edges<- unname(do.call(c, lapply(g_by_t, `[[`, 1)))
  edges<- lapply(seq_along(edges), function(i) {
    e<- edges[[i]]
    e$to<- i
    return(e)
  })

  return(new(
    "dag",
    edges = edges,
    distances = unname(do.call(c, lapply(g_by_t, `[[`, 2))),
    distance_units = distance_units(settings)
  ))
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
