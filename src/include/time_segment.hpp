vector<int> get_time_segment(vector<int> times, int this_time) {
  vector<int> segment(2);
  segment << 0, 0;
  int i=0;
  while( i<times.size() && times(i) < this_time ) i++;
  segment(0) = i;
  while( i<times.size() && times(i) == this_time ) {
    segment(1)++;
    i++;
  }
  return segment;
}

template<class Type>
matrix<Type> matrix_row_segment(matrix<Type> full_matrix, int position, int size) {
  matrix<Type> small_matrix(size,full_matrix.cols());
  for(int i=0; i<size; i++) {
    small_matrix.row(i) = full_matrix.row(i+position);
  }
  return small_matrix;
}
