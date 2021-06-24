// Read in objects of class dag from R

// Get edge list for graphs
// R -- list of of elements of the form
//   [["to"]] int
//   [["from"]] int
// C++ -- vector of integer vectors
template<class Type>
struct directed_graph {
    vector<vector<vector<int> > > dag;
    directed_graph(SEXP edge_list) {
        dag.resize(LENGTH(edge_list));
        for(int i=0; i<LENGTH(edge_list); i++) {
          SEXP v = VECTOR_ELT(edge_list,i);
          dag(i).resize(2);

          vector<int> to = asVector<int>(VECTOR_ELT(v,0));
          dag(i)(0).resizeLike(to); dag(i)(0) = to;

          vector<int> from = asVector<int>(VECTOR_ELT(v,1));
          dag(i)(1).resizeLike(from);  dag(i)(1) = from;
        }
    }
};

// Get distance list for graphs
// R -- list of numeric matrices
// C++ -- vector of (templated) numeric matrices
template<class Type>
struct dag_dists {
    vector<matrix<Type> > dag_dist;
    dag_dists(SEXP dist_list) {
        dag_dist.resize(LENGTH(dist_list));
        for(int i=0; i<LENGTH(dist_list); i++) {
            SEXP m = VECTOR_ELT(dist_list,i);
            dag_dist(i) = asMatrix<Type>(m);
        }
    }
};
