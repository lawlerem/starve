template<class Type>
class tg_cache {
  private:
    vector<vector<conditional_normal<Type> > > conditional_normals;
  public:
    tg_cache(
      transient_graph<Type> tg,
      persistent_graph<Type> pg,
      covariance2<Type> cv
    );
    tg_cache() = default;

    conditional_normal<Type> operator() (int idx,int t) { return conditional_normals(t)(idx); }
};


template<class Type>
tg_cache<Type>::tg_cache(
    transient_graph<Type> tg,
    persistent_graph<Type> pg,
    covariance2<Type> cv
  ) {
  conditional_normals.resize(tg.dim_t());
  for(int t=0; t<tg.dim_t(); t++) {
    conditional_normals(t) = vector<conditional_normal<Type> >(tg.dim_g(t));
    for(int i=0; i<conditional_normals(t).size(); i++) {
      dag_node<Type> node = tg(i,t,pg).node;
      conditional_normals(t)(i) = conditional_normal<Type>(
        cv(node.d),
        node.from.size()
      );
    }
  }
}
