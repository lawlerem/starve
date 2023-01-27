template<class Type>
class tg_cache {
  private:
    vector<vector<vector<conditional_normal<Type> > > > conditional_normals; // [t[idx[v]]]
  public:
    tg_cache(
      transient_graph<Type>& tg,
      persistent_graph<Type>& pg,
      vector<covariance<Type> >& cv
    );
    tg_cache() = default;

    conditional_normal<Type> operator() (int idx, int t, int v) { return conditional_normals(t)(idx)(v); }
};


template<class Type>
tg_cache<Type>::tg_cache(
    transient_graph<Type>& tg,
    persistent_graph<Type>& pg,
    vector<covariance<Type> >& cv
  ) {
  conditional_normals.resize(tg.dim_t());
  for(int t = 0; t < tg.dim_t(); t++) {
    conditional_normals(t) = vector<vector<conditional_normal<Type> > >(tg.dim_g(t));
    for(int i = 0; i < tg.dim_g(t); i++) {
      conditional_normals(t)(i) = vector<conditional_normal<Type> >(tg.dim_v());
      for(int v = 0; v < tg.dim_v(); v++) {
        dag_node<Type> node = tg(i, t, v, pg).node;
        matrix<Type> cov_mat = cv(v)(node.d);
        for(int i = 0; i < cov_mat.rows(); i++) {
          // Add a small number to main diagonal for numerical stability
          // Make sure that the smalled eigenvalue is > 0/
          cov_mat(i, i) *= 1.001;
        }
        conditional_normals(t)(i)(v) = conditional_normal<Type>(
          cov_mat,
          node.from.size()
        );
      }
    }
  }
}
