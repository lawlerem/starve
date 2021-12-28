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
                         silent = T) {
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
                             returnDist = F,
                             sparse = T,
                             progress = !silent,
                             k = switch(obs_dag_method(settings),
                                        standard = n_neighbours(settings),
                                        mesh = 3,
                                        n_neighbours(settings)),
                            maxdist = max_distance(settings))
    })
    for( i in seq_along(nn_idx) ) {
      edge_list[[nn_idx[[i]]]]$from<- nn_list[[i]]
    }
  } else {}

  if( obs_dag_method(settings) == "mesh" &&  n_neighbours(settings) > 3 ) {
    adjacency_matrix<- extras(settings)$adjacency_matrix
    edge_list[nn_idx]<- lapply(edge_list[nn_idx],function(e) {
      to<- e$to
      from<- e$from
      while( length(from) < n_neighbours(settings) ) {
        all_neighbours<- do.call(c,lapply(from,function(j) {
          which( adjacency_matrix[,j] == 1 )
        }))
        from<- unique(c(from,all_neighbours))
      }
      return(list(to = to,
                  from = head(from,n_neighbours(settings))))
    })
  } else {}

  if( obs_dag_method(settings) == "standard" ) {
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
  } else if( obs_dag_method(settings) == "mesh" ) {
    distance_matrix<- extras(settings)$distance_matrix
    dist_list<- lapply(edge_list,function(e) {
      if( length(e$from) == 1 ) {
        matrix(0)
      } else {
        dists<- matrix(0,
                       nrow=length(e$to)+length(e$from),
                       ncol=length(e$to)+length(e$from))
        dists[-1,-1]<- distance_matrix[e$from,e$from]
        parent_dists<- sf::st_distance(x[e$to,],y[e$from[1:3],])
        parent_dists<- units::set_units(parent_dists,
                                        "m",
                                        mode="standard")
        parent_dists<- units::set_units(parent_dists,
                                        distance_units(settings),
                                        mode="standard")
        dists[1,2:4]<- units::drop_units(parent_dists)
        dists[1,5:nrow(dists)]<- sapply(5:nrow(dists),function(i) {
          path_distances<- sapply(2:4,function(j) {
            return( dists[1,j] + dists[j,i] )
          })
          return(min(path_distances))
        })
        dists[,1]<- dists[1,]

        if( !requireNamespace("MASS",quietly=T) ) {
          stop("Package MASS needed to use inla.mesh for nodes. Please install it.",
            call. = FALSE)
        } else {}
        dists<-  dist(MASS::isoMDS(dists,trace=F)$points,
                      diag = T,
                      upper = T)
        return(as.matrix(dists))
      }
    })
  }

  edge_list<- lapply(edge_list,function(e) {
    e$to<- e$to-resets[[e$to]]
    return(e)
  })
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
