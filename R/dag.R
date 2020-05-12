#' @include classes.R generics.R utility.R
NULL

#####################
###               ###
### Construct_dag ###
###               ###
#####################

#' @details The \code{initialize} function is not mean to be used by the user,
#'   use \code{dag} instead.
#'
#' @export
#' @rdname dag
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

#' Get or set slots from an object of class \code{dag}.
#'
#' @param x An object of class \code{dag}.
#' @param value A replacement value.
#'
#' @family Access_dag
#' @name Access_dag
NULL

#' Print method for objects of class \code{dag}.
#'
#' @export
#' @noRd
setMethod(f = "show",
          signature = "dag",
          definition = function(object) {
  n_nodes<- length(edges(object))
  avg_deg<- median(do.call(c,lapply(edges(object),length)))
  avg_dist<- mean(do.call(c,lapply(distances(object),c)))
  cat("\n")
  print(paste0("A directed acyclic graph with ",n_nodes,
               " nodes, with an median in-degree of ",avg_deg,"."))
  print(paste0("The average edge distance is ",round(avg_dist,2),"",
               distance_units(object),"."))

  return(invisible())
})

#' @export
setMethod(f = "edges",
          signature = "dag",
          definition = function(x) return(x@edges)
)
#' @export
setReplaceMethod(f = "edges",
                 signature = "dag",
                 definition = function(x,value) {
  x@edges<- value
  return(x)
})



#' @export
setMethod(f = "distances",
          signature = "dag",
          definition = function(x) return(x@distances)
)
#' @export
setReplaceMethod(f = "distances",
                 signature = "dag",
                 definition = function(x,value) {
  x@distances<- value
  return(x)
})



#' @export
setMethod(f = "distance_units",
          signature = "dag",
          definition = function(x) return(x@distance_units)
)
#' @export
setReplaceMethod(f = "distance_units",
                 signature = "dag",
                 definition = function(x,value) {
  x@distance_units<- value
  return(x)
})



###############
###         ###
### Utility ###
###         ###
###############

#' Get parent nodes for a node of a directed acyclic graph.
#'
#' @param x An \code{sf} object containing one point geometry (if more than one
#'   is present, only the first will be used).
#' @param nodes An \code{sf} object containing point geometries. These will
#'   be the possible parent nodes of \code{x}.
#' @param settings An object of class \code{staRVe_settings}.
#' @param silent Should intermediate calculations be shown?
#'
#' @return A list with elements "parents" and "dists". The "parents" element
#'   is a vector giving the indices of the rows of \code{nodes} which are the
#'   parents of \code{x}. The "dists" element is a matrix whose first row is the
#'   distance between \code{x} and its parents, and the rest of the matrix gives
#'   the distances between parent nodes.
.get_one_dag_node<- function(x,
                             nodes,
                             settings,
                             silent = T,
                             ...) {
  if( nrow(nodes) == 0 ) {
    parents<- numeric(0)
    dists<- matrix(0)
    return(list(parents,dists))
  } else {}

  if( nrow(nodes) <= n_neighbours(settings) ) {
    n_far_neighbours<- 0
    m<- nrow(nodes)
  } else {
    n_far_neighbours<- round(p_far_neighbours(settings)*n_neighbours(settings))
    m<- n_neighbours(settings) - n_far_neighbours
  }
  suppressMessages({
  nn_obj<- nngeo::st_nn(x = x,
                        y = nodes,
                        returnDist = T,
                        sparse = T,
                        progress = !silent,
                        k = m,
                        maxdist = max_distance(settings))
  })
  names(nn_obj)<- c("nn","dist")

  parents<- nn_obj$nn[[1]]
  if( length(parents) == 0) {
    parents<- numeric(0)
    dists<- matrix(0)
    return(list(parents,dists))
  } else {}

  far_neighbours<- sample(seq(nrow(nodes))[-parents],size = n_far_neighbours)
  parents<- c(parents,far_neighbours)

  nn_obj$dist<- matrix(nn_obj$dist[[1]],nrow=1)
  cross_dists<- cbind(nn_obj$dist, sf::st_distance(x,nodes[far_neighbours,]))
  cross_dists<- units::set_units(cross_dists,"m") # st_nn always returns meters
  cross_dists<- units::set_units(cross_dists,
                                 distance_units(settings),
                                 mode="standard")
  parent_dists<- units::set_units(sf::st_distance(nodes[parents,]),"m")
  parent_dists<- units::set_units(parent_dists,
                                  distance_units(settings),
                                  mode="standard")
  dists<- rbind(cross_dists,parent_dists)
  dists<- cbind(c(0,cross_dists),dists)

  return(list(parents,dists))
}

#' Get parent nodes for a node of a directed acyclic graph.
#'
#' @param x An \code{sf} object containing one point geometry (if more than one
#'   is present, only the first will be used).
#' @param nodes An \code{sf} object containing point geometries. These will
#'   be the possible parent nodes of \code{x}.
#' @param settings An object of class \code{staRVe_settings}.
#' @param silent Should intermediate calculations be shown?
#'
#' @return A list with elements "parents" and "dists". The "parents" element
#'   is a vector giving the indices of the rows of \code{nodes} which are the
#'   parents of \code{x}. The "dists" element is a matrix whose first row is the
#'   distance between \code{x} and its parents, and the rest of the matrix gives
#'   the distances between parent nodes.
#'
#'   If \code{x} is a point in \code{nodes} then the "parents" element is a single
#'   integer giving the row of the point in \code{nodes}, and "dists" is a 1x1
#'   zero matrix.
.get_one_intersects_dag_node<- function(x,
                                        nodes,
                                        settings,
                                        silent) {
  intersection_idx<- sf::st_equals(x,nodes)[[1]]
  if( length(intersection_idx) == 0 ) {
    node<- .get_one_dag_node(x = x,
                             nodes = nodes,
                             settings = settings,
                             silent = silent)
  } else {
    parents<- intersection_idx[[1]] # Don't want more than 1
    dists<- matrix(0)
    node<- list(parents,dists)
  }

  return(node)
}

#' Construct a directed acyclic graph for an \code{sf} object with point geometry.
#'
#' The directed acyclic graph is stored as an ordered list. The i'th element of
#'  the list is a vector containing indices j such that there is a directed edge
#'  from node j to node i. In other words, each node j is a directed parent of node i.
#'
#' There can only be an edge from vertex i to vertex j if i comes before j in
#'  the rows of \code{x}.
#'
#' @param x An \code{sf} object containing point geometries, which will be the
#'  nodes of the graph.
#' @param settings An object of class \code{staRVe_settings}.
#' @param silent Should intermediate calculations be shown?
#'
#' @return An object of class \code{dag}.
#'
#' @export
construct_dag<- function(x,
                         settings = new("staRVe_settings"),
                         silent = T) {
  max_dist<- units::set_units(max_distance(settings),
                              distance_units(settings),
                              mode="standard")
  max_distance(settings)<- as.numeric(units::set_units(max_dist,"m"))
  ### st_nn expects meters.

  nn_list<- lapply(seq(nrow(x)), function(i) {
    .get_one_dag_node(x = x[i,],
                      nodes = head(x,i-1),
                      settings = settings,
                      silent = silent)
  })
  edge_list<- lapply(nn_list,`[[`,1)
  dist_list<- lapply(nn_list,`[[`,2)
  dag<- new("dag",
            edges = edge_list,
            distances = dist_list,
            distance_units = distance_units(settings))

  return(dag)
}

#' Construct a directed acyclic graph between two \code{sf} objects with point geometry.
#'
#' The directed acyclic graph is stored as an ordered list. The i'th element of
#'  the list is a vector containing indices j such that there is a directed edge
#'  from node j (in y) to node i (in x). If any location in x is present in y,
#'  there is only a single edge connecting the location in y to the same location in x.
#'
#' @param x An \code{sf} object containing point geometries. Directed edges will
#'  enter nodes in x.
#' @param y An \code{sf} objecy containing point geometries. Directed edges will
#'  leave nodes in y.
#' @param n_neighbours An integer giving the number of parents for each node.
#' @param p_far_neighbours What percent of neighbours should be randomly selected?
#' @param check_intersection Logical. If true, if any point in \code{x} is also
#'   a point in \code{y}, then there will be exactly one edge into that point in
#'   \code{x} coming from the same point in \code{y} (with a distance of 0).
#' @param max_dist The maximum distance (in m) to search for parents.
#' @param return_units Which units should be used for distances?
#'
#' @return An object of class \code{dag}.
#'
#' @export
construct_obs_dag<- function(x,
                             y,
                             settings = new("staRVe_settings"),
                             check_intersection = T,
                             silent = T) {
  max_dist<- units::set_units(max_distance(settings),
                              distance_units(settings),
                              mode="standard")
  max_distance(settings)<- as.numeric(units::set_units(max_dist,"m"))
  ### st_nn expects meters.

  nn_list<- lapply(seq(nrow(x)), function(i) {
    args<- list(x = x[i,],
                nodes = y,
                settings = settings,
                silent = silent)
    if( check_intersection == T ) {
      do.call(.get_one_intersects_dag_node,args)
    } else {
      do.call(.get_one_dag_node,args)
    }
  })

  edge_list<- lapply(nn_list,`[[`,1)
  dist_list<- lapply(nn_list,`[[`,2)
  dag<- new("dag",
            edges = edge_list,
            distances = dist_list,
            distance_units = distance_units(settings))

  return(dag)
}

setMethod(f = "idxC_to_R",
          signature = "dag",
          definition = function(x) {
  edges(x)<- lapply(edges(x),function(parents) {
    return(parents+1)
  })
})

setMethod(f = "idxR_to_C",
          signature = "dag",
          definition = function(x) {
  edges(x)<- lapply(edges(x),function(parents) {
    return(parents-1)
  })
})
