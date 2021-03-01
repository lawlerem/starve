// Find the first occurrence of this_time, and how many times it is repeated
// e.g. [0,0,0,1,1,2] with this_time=1 gives [3,2]
vector<int> get_time_segment(vector<int> times, int this_time) {
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
matrix<Type> matrix_row_segment(matrix<Type> full_matrix, int position, int size) {
  matrix<Type> small_matrix(size,full_matrix.cols());
  for(int i=0; i<size; i++) {
    small_matrix.row(i) = full_matrix.row(i+position);
  }
  return small_matrix;
}
