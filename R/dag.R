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

#' Get parent nodes for a node of a directed acyclic graph.
#'
#' @param x An \code{sf} object containing one point geometry (if more than one
#'   is present, only the first will be used).
#' @param nodes An \code{sf} object containing point geometries. These will
#'   be the possible parent nodes of \code{x}.
#' @param n_neighbours A positive integer giving the number of parents for each node.
#' @param max_dist The maximum distance (in meters) to search for parents.
#' @param distance_units Which units should be used for distances?
#'
#' @return A list with elements "parents" and "dists". The "parents" element
#'   is a vector giving the indices of the rows of \code{nodes} which are the
#'   parents of \code{x}. The "dists" element is a matrix whose first row is the
#'   distance between \code{x} and its parents, and the rest of the matrix gives
#'   the distances between parent nodes.
.get_one_dag_node<- function(x,
                             nodes,
                             n_neighbours,
                             max_dist,
                             distance_units,
                             silent = T,
                             ...) {
  if( nrow(nodes) == 0 ) {
    parents<- numeric(0)
    dists<- matrix(0)
    return(list(parents,dists))
  } else {}

  m<- min(nrow(nodes),n_neighbours)
  nn_obj<- nngeo::st_nn(x = x,
                        y = nodes,
                        returnDist = T,
                        sparse = T,
                        progress = !silent,
                        k = m,
                        maxdist = max_dist)
  names(nn_obj)<- c("nn","dist")

  parents<- nn_obj$nn[[1]]
  if( length(parents) == 0) {
    parents<- numeric(0)
    dists<- matrix(0)
    return(list(parents,dists))
  } else {}

  nn_obj$dist<- matrix(nn_obj$dist[[1]],nrow=1)
  cross_dists<- units::set_units(nn_obj$dist,"m") # st_nn always returns meters
  cross_dists<- units::set_units(cross_dists,distance_units,mode="standard")
  parent_dists<- units::set_units(sf::st_distance(nodes[parents,]),"m")
  parent_dists<- units::set_units(parent_dists,distance_units,mode="standard")
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
#' @param n_neighbours A positive integer giving the number of parents for each node.
#' @param max_dist The maximum distance (in meters) to search for parents.
#' @param distance_units Which units should be used for distances?
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
                                        n_neighbours,
                                        max_dist,
                                        distance_units,
                                        silent) {
  intersection_idx<- sf::st_equals(x,nodes)[[1]]
  if( length(intersection_idx) == 0 ) {
    node<- .get_one_dag_node(x = x,
                             nodes = nodes,
                             n_neighbours = n_neighbours,
                             max_dist = max_dist,
                             distance_units = distance_units,
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
#' @export
construct_dag<- function(x,
                         n_neighbours = 10,
                         max_dist = Inf,
                         distance_units = "km",
                         silent = T) {
  max_dist<- units::set_units(max_dist,distance_units,mode="standard")
  max_dist<- units::set_units(max_dist,"m") ### st_nn expects meters.
  max_dist<- as.numeric(max_dist)

  nn_list<- lapply(seq(nrow(x)), function(i) {
    .get_one_dag_node(x = x[i,],
                      nodes = head(x,i-1),
                      n_neighbours = n_neighbours,
                      max_dist =  max_dist,
                      distance_units = distance_units,
                      silent = silent)
  })
  edge_list<- lapply(nn_list,`[[`,1)
  dist_list<- lapply(nn_list,`[[`,2)
  return(list(edge_list = edge_list,
              dist_list = dist_list))
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
#' @param check_intersection Logical. If true, if any point in \code{x} is also
#'   a point in \code{y}, then there will be exactly one edge into that point in
#'   \code{x} coming from the same point in \code{y} (with a distance of 0).
#' @param max_dist The maximum distance (in m) to search for parents.
#' @param return_units Which units should be used for distances?
#'
#' @return Two lists. `edge_list` is a list with an entry for each node containing
#'  the indices of the parents of that node. `dist_list` is a list with an entry
#'  for each node containing the distance between that node and its parents
#'  (first row) and the pairwise distances between its parents.
#'
#' @export
construct_obs_dag<- function(x,
                             y,
                             n_neighbours = 10,
                             check_intersection = T,
                             max_dist = Inf,
                             distance_units = "km",
                             silent = T) {
  max_dist<- units::set_units(max_dist,distance_units,mode="standard")
  max_dist<- units::set_units(max_dist,"m") ### st_nn expects meters.
  max_dist<- as.numeric(max_dist)

  nn_list<- lapply(seq(nrow(x)), function(i) {
    args<- list(x = x[i,],
                nodes = y,
                n_neighbours = n_neighbours,
                max_dist = max_dist,
                distance_units = distance_units,
                silent = silent)
    if( check_intersection == T ) {
      do.call(.get_one_intersects_dag_node,args)
    } else {
      do.call(.get_one_dag_node,args)
    }
  })

  edge_list<- lapply(nn_list,`[[`,1)
  dist_list<- lapply(nn_list,`[[`,2)
  return(list(edge_list = edge_list,
              dist_list = dist_list))
}



#' Reduce all the parent indices of a directed acyclic graph by one for use in cpp.
#'
#' @param dag A directed acyclic graph. See \code{\link{construct_dag}}.
#'
#' @return The same directed acyclic graph as passed in with all parent indices
#'  reduced by one.
#'
#' @export
Rdag_to_Cdag<- function(dag) {
    dag<- lapply(dag,function(parents) {
        return(parents-1)
    })
    return(dag)
}

#' Increase all the parent indices of a directed acyclic graph by one for use in R.
#'
#' @param dag A directed acyclic graph. See \code{\link{construct_dag}}.
#'
#' @return The same directed acyclic graph as passed in with all parent indices
#'  increased by one.
#'
#' @export
Cdag_to_Rdag<- function(dag) {
    dag<- lapply(dag,function(parents) {
        return(parents+1)
    })
    return(dag)
}
