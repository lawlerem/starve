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

#' Find Parent Nodes for a Node of a Directed Acyclic Graph.
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
#'   parents of \code{x}. The "dists" element is a symmetric matrix whose first
#'   row/column is the distance between \code{x} and its parents, and the rest
#'   of the matrix gives the distances between parent nodes.
#'
#' @noRd
.get_one_dag_node<- function(x,
                             nodes,
                             settings,
                             silent = T,
                             ...) {
  if( nrow(nodes) == 0 ) {
    # Need at least one potential parent
    parents<- numeric(0)
    dists<- matrix(0)
    return(list(parents,dists))
  } else {}

  if( nrow(nodes) <= n_neighbours(settings) ) {
    # Use all available vertices in graph
    n_far_neighbours<- 0
    m<- nrow(nodes)
  } else {
    # Determine how many parents should be taken at random instead of nearest
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
    # No parents found
    parents<- numeric(0)
    dists<- matrix(0)
    return(list(parents,dists))
  } else {}

  # If any parents are selected randomly, add them
  far_neighbours<- sample(seq(nrow(nodes))[-parents],size = n_far_neighbours)
  parents<- c(parents,far_neighbours)

  # Group all relevant distances together, and ensure they use correct units
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

#' Find Parent Nodes for a Node of a Directed Acyclic Graph
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
#'
#' @noRd
.get_one_intersects_dag_node<- function(x,
                                        nodes,
                                        settings,
                                        silent) {
  intersection_idx<- sf::st_equals(x,nodes)[[1]]
  if( length(intersection_idx) == 0 ) {
    # The location of x is not the location of any node
    node<- .get_one_dag_node(x = x,
                             nodes = nodes,
                             settings = settings,
                             silent = silent)
  } else {
    # Pick out the node whose location is the location of x
    parents<- intersection_idx[[1]] # Don't want more than 1
    dists<- matrix(0)
    node<- list(parents,dists)
  }

  return(node)
}



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
#'
#' @return An object of class \code{dag}
#'
#' @name construct_dag
NULL

#' @describeIn construct_dag Construct a directed acyclic graph from a single
#'   \code{sf} object. The first element of the ordered list contains the indices
#'   for the first k rows, where k is the n_neighbours setting. The ith element
#'   contains the parents for the (i+k-1)th row of x.
#'
#' @export
construct_dag<- function(x,
                         settings = new("staRVe_settings"),
                         silent = T) {
  # Parents won't be eligible if their distance is too far
  max_dist<- units::set_units(max_distance(settings),
                              distance_units(settings),
                              mode="standard")
  max_distance(settings)<- as.numeric(units::set_units(max_dist,"m"))
  ### st_nn expects meters.

  # Get the edges and distances for the first k nodes
  n_startup<- n_neighbours(settings)
  startupEdges<- list(to = seq(n_startup),
                      from = integer(0))
  startupDists<- units::set_units(sf::st_distance(x[do.call(c,startupEdges),]),"m")
  startupDists<- units::set_units(startupDists,
                                  distance_units(settings),
                                  mode="standard")
  startupDists<- units::drop_units(startupDists)

  # Get the edges and distances for the remaining nodes
  # nn_list<- lapply(seq(nrow(x))[-seq(n_neighbours(settings))], function(i) {
  nn_list<- lapply(seq(nrow(x))[-seq(n_startup)], function(i) {
    foo<- .get_one_dag_node(x = x[i,],
                            nodes = head(x,i-1),
                            settings = settings,
                            silent = silent)
    return(list(edges = list(to = i,
                             from = foo[[1]]),
                distances = foo[[2]]))
  })

  # Store everything in a dag object
  edge_list<- c(list(startupEdges),lapply(nn_list,`[[`,1))
  dist_list<- c(list(startupDists),lapply(nn_list,`[[`,2))
  dag<- new("dag",
            edges = edge_list,
            distances = dist_list,
            distance_units = distance_units(settings))

  return(dag)
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
                             check_intersection = T,
                             silent = T) {
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
  nn_list<- lapply(seq(nrow(x)), function(i) {
    args<- list(x = x[i,],
                nodes = y,
                settings = settings,
                silent = silent)
    if( check_intersection == T ) {
      foo<- do.call(.get_one_intersects_dag_node,args)
    } else {
      foo<- do.call(.get_one_dag_node,args)
    }
    return(list(edges = list(to = i-resets[[i]], # Make sure new years start at 0
                             from = foo[[1]]),
                distances = foo[[2]]))
  })

  # Store everything in a dag object
  edge_list<- lapply(nn_list,`[[`,1)
  dist_list<- lapply(nn_list,`[[`,2)
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





#' Convert an R-INLA mesh to a dag
#'
#' @param x An inla.mesh object. I.e., output from INLA::inla.mesh.2d
#' @param crs The coordinate system used. If missing, taken from x
#' @param n_init How many vertices should be used to start the graph?
#'   Using at least 10 is recommended.
#'
#' @export
inla.mesh_to_dag<- function(
  x,
  crs = NA,
  n_init = 10) {
  locs<- sf::st_as_sf(as.data.frame(x$loc[,c(1,2)]),coords=c(1,2))
  sf::st_crs(locs)<- crs
  m<- x$graph$vv
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
  reorder_inla_matrix<- function(
    m,
    locs,
    n_init=5,
    start=1) {
    put_v_at_k<- function(m,v,k) {
      order<- c(v,setdiff(k:nrow(m),v))
      m[k:nrow(m),]<- m[order,]
      if( ncol(m) == nrow(m) ) { # Quick check for adjacency matrix
        m[,k:nrow(m)]<- m[,order]
      } else {}
        return(m)
    }
    m0<- m; m0[lower.tri(m0)]<- 0
    if( start == 1 ) {
      ### Starting vertices
      joint<- 1
      cntr<- length(joint)
      while( length(joint) < n_init ) {
        joint<- c(joint,children(m0,joint))
        joint<- c(joint,parents(m0,joint))
        if( length(joint) == cntr ) {
          warning(paste("Could not find",n_joint,"nodes to start."))
          break
        } else { }
        cntr<- length(joint)
      }
      joint<- head(joint,n_init)
      m<- put_v_at_k(m,v=joint,k=start)
      locs<- put_v_at_k(locs,v=joint,k=start)
      return(reorder_inla_matrix(
        m=m,
        locs=locs,
        n_init=n_init,
        start=start+length(joint)))
    } else if(  1 < start && start < ncol(m) ) {
      ### First child of 1:start with maximum # of parents
      parent_length<- sapply(start:ncol(m), function(v) {
        p<- parents(m0,v)
        p<- intersect(p,1:(start-1))
        return(length(p))
      })
      v<- start-1+which.max(parent_length)
      m<- put_v_at_k(m,v=v,k=start)
      locs<- put_v_at_k(locs,v=v,k=start)
      return(reorder_inla_matrix(
        m=m,
        locs=locs,
        n_init=n_init,
        start=start+1))
    } else if( start == ncol(m) ) {
      # If we've gone through the whole matrix, we're done
        return(list(locs = locs,
                    mat = m))
    }
  }
  m_locs<- reorder_inla_matrix(m=m,locs=locs,n_init=n_init)
  m_locs$mat[lower.tri(m_locs$mat,diag=T)]<- 0

  edge_list<- vector(mode="list",length=nrow(m_locs$locs)-n_init+1)
  edge_list[[1]]<- list(to = seq(n_init),
                        from = numeric(0))

  edge_list[2:length(edge_list)]<- lapply((n_init+1):nrow(m_locs$locs),function(i) {
    return(list(to = i,
                from = parents(m_locs$mat,i)))
  })

  dist_list<- lapply(edge_list,function(edges) {
    all_v<- c(edges$to,edges$from)
    dists<- sf::st_distance(m_locs$locs[all_v,])
    return(dists)
  })

  if( "units" %in% class(dist_list[[1]]) ) {
    distance_units<- as.character(units(dist_list[[1]]))
    dist_list<- lapply(dist_list,units::drop_units)
  } else {
    warning("Could not infer distance units, assuming meters. Supply a coordinate reference system if available")
    distance_units<- "m"
  }

  return(list(nodes = m_locs$locs,
              persistent_graph = new("dag",
                edges = edge_list,
                distances = dist_list,
                distance_units = distance_units),
              edge_list = edge_list,
              dist_list = dist_list))
}
