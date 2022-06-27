template<class Type>
class nngp2 {
  private:
    persistent_graph<Type> pg;
    transient_graph<Type> tg;
    vector<covariance2<Type> > cv; // Vector elements hold covariance for different variables

    pg_cache<Type> pg_c;
    tg_cache<Type> tg_c;

    Type pg_loglikelihood(time_series<Type>& ts);
    Type tg_loglikelihood(time_series<Type>& ts);

    nngp2<Type> pg_simulate(time_series<Type>& ts);
    nngp2<Type> tg_simulate(time_series<Type>& ts);
  public:
    nngp2(
      persistent_graph<Type>& pg,
      transient_graph<Type>& tg,
      vector<covariance2<Type> >& cv
    ) : pg{pg}, tg{tg}, cv{cv}, pg_c{pg,cv}, tg_c{tg,pg,cv} {};

    int dim_g(int t) { return pg.dim_g() + tg.dim_g(t); } // number of nodes in pg + number in tg(t)
    int dim_s(int t) { return pg.dim_s() + tg.dim_s(t); } // number of locs in pg + number in tg(t)
    int dim_t() { return pg.dim_t(); } // number of times
    int dim_v() { return pg.dim_v(); } // number of vars

    array<Type> get_pg_re() { return pg.re; }
    array<Type> get_pg_mean() { return pg.mean; }
    dag<Type> get_pg_graph() { return pg.graph; }

    array<Type> get_tg_re() { return tg.get_re(); }
    array<Type> get_tg_mean() { return tg.get_mean(); }
    dag<Type> get_tg_graph() { return tg.get_graph(); }

    // Returns just the random effect for [location, time, var]
    Type operator() (int s,int t, int v) {return (s<pg.dim_s() ? pg.re(s,t,v) : tg.re(t)(s-pg.dim_s(),v));}

    Type loglikelihood(time_series<Type>& ts) { return pg_loglikelihood(ts)+tg_loglikelihood(ts); }
    nngp2<Type> simulate(time_series<Type>& ts) { pg_simulate(ts); tg_simulate(ts); return *this; }
};





template<class Type>
Type nngp2<Type>::pg_loglikelihood(time_series<Type>& ts) {
  // Create marginal means for pg
  pg.mean = ts.propagate_structure(pg.re);

  // Use pg_cache to compute loglikelihood
  Type pg_ll = 0.0;
  for(int v=0; v<pg.dim_v(); v++) {
    for(int t=0; t<pg.dim_t(); t++) {
      for(int node=0; node<pg.dim_g(); node++) {
        pg_ll += pg_c(node,v).loglikelihood(pg(node,t,v).re,pg(node,t,v).mean);
      }
    }
  }

  return pg_ll;
}


template<class Type>
nngp2<Type> nngp2<Type>::pg_simulate(time_series<Type>& ts) {
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
        re_dag_node<Type> pg_node = pg(node);
        vector<Type> to_mean = ts.propagate_structure(pg_node.re,t,v);
        pg.set_mean_by_to_g(to_mean.segment(0,pg_node.node.to.size()),node,t,v);

        pg_node = pg(node,t,v);
        // "to" nodes for pg_node.re won't be used
        vector<Type> sim = pg_c(node,v).simulate(pg_node.re,pg_node.mean);

        pg.set_re_by_to_g(sim,node,t,v);
      }
    }
  }

  return *this;
}


template<class Type>
Type nngp2<Type>::tg_loglikelihood(time_series<Type>& ts) {
  Type tg_ll = 0.0;
  for(int v=0; v<tg.dim_v(); v++) {
    for(int t=0; t<tg.dim_t(); t++) {
      for(int node=0; node<tg.dim_g(t); node++) {
        tg_ll += tg_c(node,t,v).loglikelihood(tg(node,t,v,pg).re,tg(node,t,v,pg).mean,true); // true -> interpolate mu
      }
    }
  }

  return tg_ll;
}


template<class Type>
nngp2<Type> nngp2<Type>::tg_simulate(time_series<Type>& ts) {
  /*
  for v in variables
    for t in years
      for node in tg.nodes
        1.) interpolate and store mean for tg
          ---- DO NOT need to propagate mean
        2.) simulate from conditional normal
        3.) update random effects and mean in transient graph
  */
  for(int v=0; v<tg.dim_v(); v++) {
    for(int t=0; t<tg.dim_t(); t++) {
      for(int node=0; node<tg.dim_g(t); node++) {
        re_dag_node<Type> tg_node = tg(node,t,v,pg);
        vector<Type> inter_mu = tg_c(node,t,v).interpolate_mean(tg_node.mean);
        tg.set_mean_by_to_g(inter_mu,node,t,v,pg);

        tg_node = tg(node,t,v,pg);
        vector<Type> sim = tg_c(node,t,v).simulate(tg_node.re,tg_node.mean);

        tg.set_re_by_to_g(sim,node,t,v,pg);
      }
    }
  }

  return *this;
}
