// Headers are included in staRVe.cpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template<class Type>
Type testing(objective_function<Type>* obj) {
    DATA_STRING(test);

    PARAMETER(dummy);

    if(test == "time_series") {
      DATA_ARRAY(ts_re);
      DATA_ARRAY(ts_pars);

      time_series<Type> ts(
        ts_re,
        ts_pars
      );

      array<Type> small_t_re = ts.slice_t(2,3).get_re();
      REPORT(small_t_re);
      array<Type> small_v_re = ts.slice_v(0,1).get_re();
      REPORT(small_v_re);

      // Type loglikelihood = ts.slice_v(0,1).slice_t(0,1).loglikelihood();
      Type loglikelihood = ts.loglikelihood();
      REPORT(loglikelihood);
      array<Type> sim = ts.simulate().get_re();
      REPORT(sim);
    } else if(test == "persistent_graph") {
      DATA_ARRAY(pg_re); // [space,time,var]
      DATA_STRUCT(pg_edges,directed_graph);
      DATA_STRUCT(pg_dists,dag_dists);

      dag<Type> pg_g(pg_edges.dag,pg_dists.dag_dist);

      persistent_graph<Type> pg(
        pg_re,
        pg_re,
        pg_g
      );

      vector<int> idx(3);
      idx << 0,2,4;
      array<Type> small_s_re = pg.subset_re_by_s(idx);
      REPORT(small_s_re);
      array<Type> small_s_mean = pg.subset_mean_by_s(idx);
      REPORT(small_s_mean);

      persistent_graph_node<Type> pgnode = pg(1);
      array<Type> small_g_re = pgnode.re;
      array<Type> small_g_mean = pgnode.mean;
      matrix<Type> small_g_di = pgnode.node.d;
      REPORT(small_g_re);
      REPORT(small_g_mean);
      REPORT(small_g_di);

      persistent_graph_node<Type> one_node = pg(1,0,0);
      array<Type> one_g_re = one_node.re;
      array<Type> one_g_mean = one_node.mean;
      REPORT(one_g_re);
      REPORT(one_g_mean);

      array<Type> small_t_re = pg.slice_t(1,2).get_re();
      array<Type> small_t_mean = pg.slice_t(1,2).get_mean();
      REPORT(small_t_re);
      REPORT(small_t_mean);

      array<Type> small_v_re = pg.slice_v(1,1).get_re();
      array<Type> small_v_mean = pg.slice_v(1,1).get_mean();
      REPORT(small_v_re);
      REPORT(small_v_mean);


      vector<Type> new_vals(pg(0).node.to.size());
      for(int i=0; i<new_vals.size(); i++) {
        new_vals(i) = -1.0*Type(i+1)/2.0;
      }
      array<Type> overwrite_re = pg.set_re_by_to_g(new_vals,0,0,0).get_re();
      REPORT(overwrite_re);
      array<Type> after_overwrite_re = pg.get_re();
      REPORT(after_overwrite_re);

      vector<Type> overwrite_re1 = pg(0,0,0).re;
      REPORT(overwrite_re1);

      new_vals << -20.0, -21.0;
      array<Type> overwrite_mean = pg.set_mean_by_to_g(new_vals,0,0,0).get_mean();
      REPORT(overwrite_mean);
      array<Type> after_overwrite_mean = pg.get_mean();
      REPORT(after_overwrite_mean);

      vector<Type> overwrite_mean1 = pg(0,0,0).mean;
      REPORT(overwrite_mean1);
    } else if(test == "transient_graph" ) {
      DATA_ARRAY(pg_re); // [space,time,var]
      DATA_STRUCT(pg_edges,directed_graph);
      DATA_STRUCT(pg_dists,dag_dists);
      dag<Type> pg_g(pg_edges.dag,pg_dists.dag_dist);

      persistent_graph<Type> pg(
        pg_re,
        pg_re,
        pg_g
      );

      DATA_ARRAY(tg_re);
      DATA_STRUCT(tg_edges,directed_graph);
      DATA_STRUCT(tg_dists,dag_dists);
      dag<Type> tg_g(tg_edges.dag,tg_dists.dag_dist);

      DATA_IVECTOR(t);

      transient_graph<Type> tg(
        tg_re,
        tg_re,
        tg_g,
        t,
        pg.dim_t() // # of times
      );

      array<Type> foo = tg.get_re();
      REPORT(foo);

      array<Type> small_t_re = tg.slice_t(0,1).get_re();
      REPORT(small_t_re);

      array<Type> small_v_re = tg.slice_v(1,1).get_re();
      REPORT(small_v_re);

      transient_graph_node<Type> tg_node = tg(2,0,pg);
      array<Type> small_tg_re = tg_node.re;
      matrix<Type> small_tg_di = tg_node.node.d;
      REPORT(small_tg_re);
      REPORT(small_tg_di);
    } else if(test == "conditional_normal") {
      DATA_VECTOR(x);
      DATA_VECTOR(mu);
      DATA_MATRIX(sigma);

      conditional_normal<Type> cn(sigma,2);
      vector<Type> conditional_mean = cn.conditional_mean(x,mu);
      REPORT(conditional_mean);
      matrix<Type> conditional_sigma = cn.conditional_cov();
      REPORT(conditional_sigma);
      Type loglikelihood = cn.loglikelihood(x,mu);
      REPORT(loglikelihood);
      vector<Type> sim = cn.simulate(x,mu);
      REPORT(sim);

      conditional_normal<Type> cn_zero(sigma,0);
      vector<Type> marginal_mean = cn_zero.conditional_mean(x,mu);
      REPORT(marginal_mean);
      matrix<Type> marginal_sigma = cn_zero.conditional_cov();
      REPORT(marginal_sigma);
    } else if(test == "nngp") {
      DATA_ARRAY(ts_re);
      DATA_ARRAY(ts_pars);

      time_series<Type> ts(
        ts_re,
        ts_pars
      );

      DATA_ARRAY(pg_re); // [space,time,var]
      DATA_STRUCT(pg_edges,directed_graph);
      DATA_STRUCT(pg_dists,dag_dists);
      dag<Type> pg_g(pg_edges.dag,pg_dists.dag_dist);

      persistent_graph<Type> pg(
        pg_re,
        pg_re,
        pg_g
      );

      DATA_ARRAY(tg_re);
      DATA_STRUCT(tg_edges,directed_graph);
      DATA_STRUCT(tg_dists,dag_dists);
      dag<Type> tg_g(tg_edges.dag,tg_dists.dag_dist);

      DATA_IVECTOR(t);

      transient_graph<Type> tg(
        tg_re,
        tg_re,
        tg_g,
        t,
        pg.dim_t() // # of times
      );

      DATA_ARRAY(cv_pars);
      DATA_IVECTOR(cv_code);

      vector<covariance2<Type> > cv(pg.dim_v());
      for(int v=0; v<pg.dim_v(); v++) {
        cv(v) = covariance2<Type>(vector<Type>(cv_pars.col(v)),cv_code(v));
      }

      nngp2<Type> process(
        pg,
        tg,
        cv
      );

      array<Type> nngp_pg_re = process.get_pg_re();
      REPORT(nngp_pg_re);

      array<Type> ts_sim = ts.simulate().get_re();
      REPORT(ts_sim);
      array<Type> sim_nngp_pg_re = process.simulate(ts).get_pg_re();
      REPORT(sim_nngp_pg_re);

    } else {}


    Type nll = -1.0*pow(dummy,2);
    return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
