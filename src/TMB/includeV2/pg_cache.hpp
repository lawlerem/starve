template<class Type>
class pg_cache {
  private:
    vector<conditional_normal<Type> > conditional_normals;
  public:
    pg_cache(
      persistent_graph<Type> pg,
      covariance2<Type> cv
    );
    pg_cache() = default;

    conditional_normal<Type> operator() (int idx) { return conditional_normals(idx); }
};


template<class Type>
pg_cache<Type>::pg_cache(
    persistent_graph<Type> pg,
    covariance2<Type> cv
  ) {
    conditional_normals.resize(pg.dim_g());

    // Fill in cache for nodes with parents
    Type avg_sd = 0.0;
    int avg_n = 0;
    for(int i=0; i<conditional_normals.size(); i++) {
      dag_node<Type> node = pg(i).node;
      if( node.from.size() == 0 ) {
        // No parent nodes
        // Skip for now so we can get average forecast variance
        continue;
      } else {
        conditional_normals(i) = conditional_normal<Type>(
          cv(node.d),
          node.from.size()
        );
        for(int j=0; j<node.to.size(); j++) {
          avg_sd += sqrt(conditional_normals(i).conditional_cov()(j,j));
        }
        avg_n += node.to.size();
      }
    }

    // Fill in cache for nodes without parents
    if( avg_n > 0 ) {
      avg_sd = avg_sd / avg_n;
      cv.update_marginal_sd(avg_sd);
    } else {}
    for(int i=0; i<conditional_normals.size(); i++) {
      dag_node<Type> node = pg(i).node;
      if( node.from.size() == 0 ) {
        conditional_normals(i) = conditional_normal<Type>(
          cv(node.d),
          node.from.size()
        );
      } else {
        // Already took care of these
        continue;
      }
    }
}
