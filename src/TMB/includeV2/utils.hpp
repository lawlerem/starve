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





// Find the first occurrence of this_time, and how many times it is repeated
// e.g. [0,0,0,1,1,2] with this_time=1 gives [3,2]
vector<int> get_time_segment(const vector<int>& times, int this_time) {
  vector<int> segment(2);
  segment << 0, 0;
  int i=0;
   // Run through times until you hit a this_time (or the end)
  while( i<times.size() && times(i) < this_time ) i++;
  segment(0) = i;
  // Continue until you hit the next time (or the end)
  while( i<times.size() && times(i) == this_time ) {
    segment(1)++;
    i++;
  }
  return segment;
}

// Subset rows of a matrix by start point and number of rows wanted
template<class Type>
matrix<Type> matrix_row_segment(const matrix<Type>& full_matrix, int position, int size) {
  matrix<Type> small_matrix(size,full_matrix.cols());
  for(int i=0; i<size; i++) {
    small_matrix.row(i) = full_matrix.row(i+position);
  }
  return small_matrix;
}


template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}
