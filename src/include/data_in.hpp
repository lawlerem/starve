template<class Type>
struct directed_graph {
    vector<vector<int> > dag; // vector of edges into node
    directed_graph(SEXP edge_list) {
        dag.resize(LENGTH(edge_list));
        for(int i=0; i<LENGTH(edge_list); i++) {
            SEXP v = VECTOR_ELT(edge_list,i);
            dag(i) = asVector<int>(v);
        }
    }
};

template<class Type>
struct dag_dists {
    vector<matrix<Type> > dag_dist; // vector of edges into node
    dag_dists(SEXP dist_list) {
        dag_dist.resize(LENGTH(dist_list));
        for(int i=0; i<LENGTH(dist_list); i++) {
            SEXP m = VECTOR_ELT(dist_list,i);
            dag_dist(i) = asMatrix<Type>(m);
        }
    }
};
