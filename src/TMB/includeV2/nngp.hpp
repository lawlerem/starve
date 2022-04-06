template<class Type>
class nngp2 {
  private:
    persistent_graph<Type> pg;
    transient_graph<Type> tg;
    vector<covariance2<Type> > cv; // Vector elements hold covariance for different variables

    vector<pg_cache<Type> > pg_c;
    vector<tg_cache<Type> > tg_c;

    Type pg_loglikelihood(time_series<Type> ts);
    Type tg_loglikelihood(time_series<Type> ts);

    nngp2<Type> pg_simulate(time_series<Type> ts);
    nngp2<Type> tg_simulate(time_series<Type> ts);
  public:
    nngp2(
      persistent_graph<Type> pg,
      transient_graph<Type> tg,
      vector<covariance2<Type> > cv
    );

    array<Type> get_pg_re() { return pg.get_re(); }

    Type loglikelihood(time_series<Type> ts) { return pg_loglikelihood(ts)+tg_loglikelihood(ts); }
    // nngp2<Type> simulate(time_series<Type> ts) { return pg_simulate(ts).tg_simulate(ts); }
    nngp2<Type> simulate(time_series<Type> ts) { return pg_simulate(ts); }
};


template<class Type>
nngp2<Type>::nngp2(
    persistent_graph<Type> pg,
    transient_graph<Type> tg,
    vector<covariance2<Type> > cv
  ) :
  pg(pg),
  tg(tg),
  cv(cv) {
  // Set up persistent graph cache
  pg_c = vector<pg_cache<Type> >(cv.size());
  tg_c = vector<tg_cache<Type> >(cv.size());
  for(int v=0; v<cv.size(); v++) {
    pg_c(v) = pg_cache<Type>(pg,cv(v));
    tg_c(v) = tg_cache<Type>(tg,pg,cv(v));
  }
}


template<class Type>
Type nngp2<Type>::pg_loglikelihood(time_series<Type> ts) {
  // Create marginal means for pg
  pg.set_mean(ts.propagate_structure(pg.get_re()));

  // Use pg_cache to compute loglikelihood
  Type pg_ll = 0.0;
  for(int v=0; v<pg.dim_v(); v++) {
    for(int t=0; t<pg.dim_t(); t++) {
      for(int node=0; node<pg.dim_g(); node++) {
        persistent_graph_node<Type> pg_node = pg(node,t,v);
        pg_ll += pg_c(v)(node).loglikelihood(pg_node.re,pg_node.mean);
      }
    }
  }

  return pg_ll;
}


template<class Type>
nngp2<Type> nngp2<Type>::pg_simulate(time_series<Type> ts) {
  /*
  for v in variables
    for t in years
      for node in pg.nodes
        1.) Use ts.propagate_structure to update mean for year t in persistent graph
        2.) Simulate random effects for node and year t
        3.) Update random effects for year t in persistent graph
  */
  for(int v=0; v<pg.dim_v(); v++) {
    for(int t=0; t<pg.dim_t(); t++) {
      for(int node=0; node<pg.dim_g(); node++) {
        persistent_graph_node<Type> pg_node = pg(node,t,v);
        vector<Type> to_mean = ts.propagate_structure(pg.subset_re_by_s(pg_node.node.to),t,v);

        // "to" nodes for pg_node.re won't be used
        vector<Type> sim = pg_c(v)(node).simulate(pg_node.re,to_mean);
        pg.set_mean_by_to_g(to_mean,node,t,v);
        pg.set_re_by_to_g(sim,node,t,v);
      }
    }
  }

  return *this;
}


template<class Type>
Type nngp2<Type>::tg_loglikelihood(time_series<Type> ts) {
  Type tg_ll = 0.0;
  for(int v=0; v<tg.dim_v(); v++) {
    for(int t=0; t<tg.dim_t(); t++) {
      for(int node=0; node<tg.dim_g(t); node++) {
        transient_graph_node<Type> tg_node = tg(node,t,v,pg);
        tg_ll += tg_c(v)(node,t)(tg_node.re,tg_node.mean,true); // true -> interpolate mu
      }
    }
  }

  return tg_ll;
}


// template<class Type>
// nngp2<Type> nngp2<Type>::tg_simulate(time_series<Type> ts) {
//   /*
//   for v in variables
//     for t in years
//       for node in tg.nodes
//
//   */
// }
