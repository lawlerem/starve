// Read in objects of class dag from R

// Get edge list for graphs
// R -- list of integer vectors
// C++ -- vector of integer vectors
template<class Type>
struct directed_graph {
    vector<vector<int> > dag;
    directed_graph(SEXP edge_list) {
        dag.resize(LENGTH(edge_list));
        for(int i=0; i<LENGTH(edge_list); i++) {
            SEXP v = VECTOR_ELT(edge_list,i);
            dag(i) = asVector<int>(v);
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
