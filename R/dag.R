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
#' @rdname staRVe-construct
setMethod(
  f = "initialize",
  signature = "dag",
  definition = function(.Object,
                        edges = list(),
                        distances = list(),
                        distance_units = "km") {
    edges(.Object)<- edges
    distances(.Object)<- distances
    distance_units(.Object)<- distance_units

    return(.Object)
  }
)


##################
###            ###
### Access_dag ###
###            ###
##################

#' @param x An object
#'
#' @export
#' @describeIn dag Get edge list
setMethod(f = "edges",
          signature = "dag",
          definition = function(x) return(x@edges)
)
setReplaceMethod(f = "edges",
                 signature = "dag",
                 definition = function(x,value) {
  x@edges<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn dag Get list of edge distances
setMethod(f = "distances",
          signature = "dag",
          definition = function(x) return(x@distances)
)
setReplaceMethod(f = "distances",
                 signature = "dag",
                 definition = function(x,value) {
  x@distances<- value
  return(x)
})



#' @param x An object
#'
#' @export
#' @describeIn dag Get distance units
setMethod(f = "distance_units",
          signature = "dag",
          definition = function(x) return(x@distance_units)
)
#' @param x An object
#' @param value A replacement value
#' @export
#' @describeIn dag Set distance units. Distances are automatically converted
#'   to the new units.
setReplaceMethod(f = "distance_units",
                 signature = "dag",
                 definition = function(x,value) {
  if( length(distance_units(x)) > 0 ) {
    # Only convert units if units were previously set
    dists<- distances(x)
    dists<- lapply(dists,function(mat) {
      mat<- units::set_units(mat,distance_units(x),mode="standard")
      mat<- units::set_units(mat,value,mode="standard")
      mat<- units::set_units(mat,NULL)
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
#' @description
#' A directed acyclic graph is stored as an ordered list. The ith element of the
#'   list is a vector containing indices j such that there is a directed edge
#'   from node j to node i, or possibly node (i+k-1) as detailed below.
#'
#' @param x An \code{sf} object of point geometries
#' @param y An \code{sf} object of point geometries containing possible parents
#' @param settings An object of class \code{staRVe_settings}
#' @param check_intersection Logical. If true, duplicated locations will be checked
#' @param silent Should intermediate calculations be shown?
#' @param time The time index for each location in x

#'
#' @return An object of class \code{dag}
#'
#' @name construct_dag
NULL

#' @describeIn construct_dag Construct a directed acyclic graph from a single
#'   \code{sf} object. The first element of the ordered list contains the indices
#'   for the first k rows, where k is the n_neighbours settings. The ith element
#'   contains the parents for the (i+k-1)th row of x. Also returns a sorted copy
#'   of the locations x.
#'
#' @export
construct_dag<- function(x,
                         settings = new("staRVe_settings"),
                         silent = TRUE) {
  dist_matrix<- as.matrix(units::set_units(sf::st_distance(x),
                                           distance_units(settings),
                                           mode="standard"))
  dag<- .rcpp_dist_to_dag(d=dist_matrix,n_neighbours=min(nrow(x),n_neighbours(settings)))
  x<- x[dag$order+1,]
  dag<- new("dag",
            edges = dag$edge_list,
            distances = dag$dist_list,
            distance_units = distance_units(settings))
  dag<- idxC_to_R(dag)
  return(list(locations = x,
              dag = dag))
}

#' @describeIn construct_dag Construct a directed acyclic graph from one \code{sf}
#'   object to another. Output is a list whose t'th element is the transient graph
#'   for time t. y should be the persistent graph locations, x should be data locations.
construct_transient_dag<- function(x,
                                   y,
                                   time = 0,
                                   settings = new("staRVe_settings"),
                                   silent = TRUE) {
  if( nrow(x) == 0 ) {
    return(new("dag",distance_units=distance_units(settings)))
  } else {}
  # Make sure geometry columns are named the same
  colnames(y)[colnames(y) == attr(y,"sf_column")]<- attr(x,"sf_column")
  st_geometry(y)<- attr(x,"sf_column")

  # Parents won't be eligible if their distance is too far
  max_dist<- units::set_units(max_distance(settings),
                              distance_units(settings),
                              mode="standard")
  max_distance(settings)<- as.numeric(units::set_units(max_dist,"m"))
  ### st_nn expects meters.

  if( length(time) == 1 ) {
    time<- rep(0,nrow(x))
  } else if( length(time) != nrow(x) ) {
    stop("Time needs to be the same length as x.")
  }

  splitx<- split(x,factor(time-min(time)+1,levels=seq(max(time)-min(time)+1)),drop=FALSE)
  tg<- lapply(splitx,function(t_x) {
    if( nrow(t_x) == 0 ) {
      return( NULL )
    } else {}

    # Remove any locations of t_x already in y
    t_x<- t_x[lengths(st_equals(t_x,y)) == 0,]
    if( nrow(t_x) == 0 ) {
      return( NULL )
    } else {}

    # Get nearest neighbours
    nn_list<- suppressMessages(nngeo::st_nn(x = t_x,
                           y = y,
                           returnDist = FALSE,
                           sparse = TRUE,
                           progress = !silent,
                           k = n_neighbours(settings),
                           maxdist = max_distance(settings)))
    edge_list<- lapply(seq(nrow(t_x)),function(i) {
      return(list(to = i,
                  from = nn_list[[i]]))
    })
    dist_list<- lapply(edge_list,function(e) {
      dists<- sf::st_distance(rbind(t_x[e$to,attr(x,"sf_column")],
                                    y[e$from,attr(y,"sf_column")]))
      dists<- units::set_units(dists,
                               "m", # Set to m to line up with nngeo default
                               mode="standard")
      dists<- units::set_units(dists,
                               distance_units(settings),
                               mode="standard")
      return(units::drop_units(dists))
    })

    return(list(edges=edge_list,
                distances = dist_list))
  })


  return(new("dag",
             edges = unname(do.call(c,lapply(tg,`[[`,1))),
             distances = unname(do.call(c,lapply(tg,`[[`,2))),
             distance_units = distance_units(settings)))
}

#' @describeIn construct_dag Construct a directed acyclic graph from one \code{sf}
#'   object to another. The i'th element of the list is a vector containing
#'   indices j such that there is a directed edge from node j (in y) to node i (in x).
#'
#' @export
construct_obs_dag<- function(x,
                             y,
                             time = 0,
                             settings = new("staRVe_settings"),
                             check_intersection = TRUE,
                             remove_intersection = FALSE,
                             silent = TRUE) {
  colnames(y)[colnames(y) == attr(y,"sf_column")]<- attr(x,"sf_column")
  st_geometry(y)<- attr(x,"sf_column")
  # Parents won't be eligible if their distance is too far
  max_dist<- units::set_units(max_distance(settings),
                              distance_units(settings),
                              mode="standard")
  max_distance(settings)<- as.numeric(units::set_units(max_dist,"m"))
  ### st_nn expects meters.

  if( length(time) == 1 ) {
    time<- rep(0,nrow(x))
  } else if( length(time) != nrow(x) ) {
    stop("Time needs to be the same length as x.")
  }
  runs<- rle(time)
  runs$values<- cumsum(head(c(0,runs$lengths),-1))
  resets<- inverse.rle(runs)

  # Find the parents for each node in x
  if( check_intersection ) {
    edge_list<- st_equals(x,y)
    edge_list<- lapply(seq_along(edge_list),function(i) {
      return(list(to = i,
                  from = edge_list[[i]]))
    })
  } else {
    edge_list<- vector(nrow(x),mode="list")
    edge_list<- lapply(seq_along(edge_list),function(i) {
      return(list(to = i,
                  from = numeric(0)))
    })
  }
  nn_idx<- which(do.call(c,lapply(edge_list,function(e) return(length(e$from))) ) == 0)
  if( length(nn_idx) > 0 ) {
    suppressMessages({
      nn_list<- nngeo::st_nn(x = x[nn_idx,],
                             y = y,
                             returnDist = FALSE,
                             sparse = TRUE,
                             progress = !silent,
                             k = n_neighbours(settings),
                             maxdist = max_distance(settings))
    })
    for( i in seq_along(nn_idx) ) {
      edge_list[[nn_idx[[i]]]]$from<- nn_list[[i]]
    }
  } else {}

  dist_list<- lapply(edge_list,function(e) {
    if( length(e$from) == 1 ) {
      matrix(0)
    } else {
      dists<- sf::st_distance(rbind(x[e$to,attr(x,"sf_column")],
                                    y[e$from,attr(y,"sf_column")]))
      dists<- units::set_units(dists,
                               "m", # Set to m to line up with nngeo default
                               mode="standard")
      dists<- units::set_units(dists,
                               distance_units(settings),
                               mode="standard")
      return(units::drop_units(dists))
    }
  })

  edge_list<- lapply(edge_list,function(e) {
    e$to<- e$to-resets[[e$to]]
    return(e)
  })

  if( remove_intersection ) {
    edge_list<- edge_list[nn_idx]
    dist_lsit<- dist_list[nn_idx]
  } else {}


  dag<- new("dag",
    edges = edge_list,
    distances = dist_list,
    distance_units = distance_units(settings))

  return(dag)
}


#' Add one to each integer in edges
#'
#' @noRd
setMethod(f = "idxC_to_R",
          signature = "dag",
          definition = function(x) {
  edges(x)<- lapply(edges(x),function(e) {
    e[["to"]]<- e[["to"]]+1
    e[["from"]]<- e[["from"]]+1
    return(e)
  })
  return(x)
})
#' Subtract one from each integer in edges
#'
#' @noRd
setMethod(f = "idxR_to_C",
          signature = "dag",
          definition = function(x) {
  edges(x)<- lapply(edges(x),function(e) {
    e[["to"]]<- e[["to"]]-1
    e[["from"]]<- e[["from"]]-1
    return(e)
  })
  return(x)
})
