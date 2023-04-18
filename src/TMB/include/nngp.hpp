template<class Type>
class nngp {
  private:
    persistent_graph<Type> pg;
    transient_graph<Type> tg;
    vector<covariance<Type> > cv; // Vector elements hold covariance for different variables

    pg_cache<Type> pg_c;
    tg_cache<Type> tg_c;

    Type pg_loglikelihood(time_series<Type>& ts);
    nngp<Type> pg_simulate(time_series<Type>& ts);

    void update_tg_mean();
    Type tg_loglikelihood(time_series<Type>& ts);
    nngp<Type> tg_simulate(time_series<Type>& ts);

  public:
    nngp(
      persistent_graph<Type>& pg,
      transient_graph<Type>& tg,
      vector<covariance<Type> >& cv
    ) : pg{pg}, tg{tg}, cv{cv}, pg_c{pg, cv}, tg_c{tg, pg, cv} {};

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
    Type operator() (int s,int t, int v) {
      return (
        s < pg.dim_s() ?
        pg.re(s, t, v) :
        tg.re(t)(s - pg.dim_s(), v)
      );
    }
    Type mean(int s, int t, int v) {
      return (
        s < pg.dim_s() ?
        pg.mean(s, t, v) :
        tg.mean(t)(s - pg.dim_s(), v)
      );
    }

    Type loglikelihood(time_series<Type>& ts) { return pg_loglikelihood(ts) + tg_loglikelihood(ts); }
    nngp<Type> simulate(time_series<Type>& ts) { pg_simulate(ts); tg_simulate(ts); return *this; }

    Type prediction_loglikelihood(
      dag<Type>& pred_g,
      vector<int>& pred_t,
      array<Type>& pred_re,
      time_series<Type>& ts
    );
};





template<class Type>
Type nngp<Type>::pg_loglikelihood(time_series<Type>& ts) {
  // Create marginal means for pg
  pg.mean = ts.propagate_structure(pg.re);

  // Use pg_cache to compute loglikelihood
  Type pg_ll = 0.0;
  for(int v = 0; v < pg.dim_v(); v++) {
    for(int t = 0; t < pg.dim_t(); t++) {
      for(int node = 0; node < pg.dim_g(); node++) {
        pg_ll += pg_c(node, v).loglikelihood(
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * pg(node, t, v).re,
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * pg(node, t, v).mean
        );
      }
    }
  }

  return pg_ll;
}


template<class Type>
nngp<Type> nngp<Type>::pg_simulate(time_series<Type>& ts) {
  /*
  for v in variables
    for t in years
      for node in pg.nodes
        1.) Use ts.propagate_structure to update mean for year t in persistent graph
        2.) Simulate random effects for node and year t
        3.) Update random effects for year t in persistent graph
  */
  for(int v = 0; v < pg.dim_v(); v++) {
    for(int t = 0; t < pg.dim_t(); t++) {
      for(int node = 0; node < pg.dim_g(); node++) {

        re_dag_node<Type> pg_node = pg(node);
        vector<Type> to_mean = ts.propagate_structure(pg_node.re, t, v);
        pg.set_mean_by_to_g(
          to_mean.segment(0, pg_node.node.to.size()),
          node,
          t,
          v
        );

        pg_node = pg(node, t, v);
        // "to" nodes for pg_node.re won't be used
        vector<Type> sim = pg_c(node, v).simulate(
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * pg_node.re,
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * pg_node.mean
        );
        sim.segment(0, pg_node.node.to.size()) = (t == 0 ? ts.initial_sd_scale(v) : 1.0) * sim.segment(0, pg_node.node.to.size());

        pg.set_re_by_to_g(sim, node, t, v);
      }
    }
  }

  return *this;
}


template<class Type>
void nngp<Type>::update_tg_mean() {
  for(int v = 0; v < tg.dim_v(); v++) {
    for(int t = 0; t < tg.dim_t(); t++) {
      for(int node = 0; node < tg.dim_g(t); node++) {
        vector<Type> new_mean = tg_c(node, t, v).interpolate_mean(tg(node, t, v, pg).mean);
        tg.set_mean_by_to_g(new_mean, node, t, v, pg);
      }
    }
  }
}

template<class Type>
Type nngp<Type>::tg_loglikelihood(time_series<Type>& ts) {
  update_tg_mean();
  Type tg_ll = 0.0;
  for(int v = 0; v < tg.dim_v(); v++) {
    for(int t = 0; t < tg.dim_t(); t++) {
      for(int node = 0; node < tg.dim_g(t); node++) {
        tg_ll += tg_c(node, t, v).loglikelihood(
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * tg(node, t, v, pg).re,
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * tg(node, t, v, pg).mean
        );
      }
    }
  }

  return tg_ll;
}


template<class Type>
nngp<Type> nngp<Type>::tg_simulate(time_series<Type>& ts) {
  /*
  for v in variables
    for t in years
      for node in tg.nodes
        1.) interpolate and store mean for tg
          ---- DO NOT need to propagate mean
        2.) simulate from conditional normal
        3.) update random effects and mean in transient graph
  */
  update_tg_mean();
  for(int v = 0; v < tg.dim_v(); v++) {
    for(int t = 0; t < tg.dim_t(); t++) {
      for(int node = 0; node < tg.dim_g(t); node++) {
        re_dag_node<Type> tg_node = tg(node, t, v, pg);
        vector<Type> sim = tg_c(node, t, v).simulate(
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * tg_node.re,
          (t == 0 ? 1.0 / ts.initial_sd_scale(v) : 1.0 ) * tg_node.mean
        );
        sim.segment(0, tg_node.node.to.size()) = (t == 0 ? ts.initial_sd_scale(v) : 1.0) * sim.segment(0, tg_node.node.to.size());

        tg.set_re_by_to_g(sim, node, t, v, pg);
      }
    }
  }

  return *this;
}


template<class Type>
Type nngp<Type>::prediction_loglikelihood(
    dag<Type>& pred_g,
    vector<int>& pred_t,
    array<Type>& pred_re,
    time_series<Type>& ts) {
  Type pred_ll = 0.0;
  for(int i = 0; i < pred_g.size(); i++) {
    if( pred_g(i).from.size() == 1 ) {
      continue; // from.size() == 1  --> random effect is mapped
    } else {}

    // Consolidate random effects into a re_dag_node
    int n = pred_g(i).to.size() + pred_g(i).from.size();
    re_dag_node<Type> node {
      array<Type>(n, dim_v()),
      array<Type>(n, dim_v()),
      pred_g(i)
    };

    for(int j = 0; j < node.node.to.size(); j++) {
      for(int v = 0; v < dim_v(); v++) {
        node.re(j, v) = pred_re(node.node.to(j), v);
        node.mean(j, v) = 0.0; // Mean will be interpolated
      }
    }
    for(int j = 0; j < node.node.from.size(); j++) {
      for(int v = 0; v < dim_v(); v++) {
        node.re(j + node.node.to.size(), v) = operator() (node.node.from(j), pred_t(i), v);
        node.mean(j + node.node.to.size(), v) = mean(node.node.from(j), pred_t(i), v);
      }
    }
    // Create a conditional normal, interpolate mean, and compute loglikelihood
    for(int v = 0; v < dim_v(); v++) {
      matrix<Type> cov_mat = (pred_t(i) == 0 ? 1.0 * ts.initial_sd_scale(v) : 1.0 ) * cv(v)(node.node.d);
        for(int j = 0; j < cov_mat.rows(); j++) {
          // Add a small number to main diagonal for numerical stability
          // Make sure that the smalled eigenvalue is > 0/
          cov_mat(j, j) *= 1.001;
        }
      conditional_normal<Type> cn {cov_mat, static_cast<int>(node.node.from.size())};
      node.mean.col(v) = cn.interpolate_mean(node.mean.col(v));
      pred_ll += cn.loglikelihood(node.re.col(v), node.mean.col(v));
    }
  }

  return pred_ll;
}
