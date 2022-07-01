// Headers are included in staRVe.cpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template<class Type>
Type testing(objective_function<Type>* obj) {
    DATA_STRING(test);

    PARAMETER(dummy);


    if(test == "covariance") {
      DATA_VECTOR(pars);
      DATA_INTEGER(covar_code);
      DATA_MATRIX(d);

      covariance<Type> cv {pars, covar_code};

      matrix<Type> sigma = cv(d);
      REPORT(sigma);

    } else if(test == "conditional_normal") {
      DATA_VECTOR(x);
      DATA_VECTOR(mu);
      DATA_MATRIX(sigma);

      conditional_normal<Type> cn {sigma, 2};
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




    } else if(test == "time_series") {
      DATA_ARRAY(ts_re);
      DATA_ARRAY(ts_pars);

      time_series<Type> ts {ts_re, ts_pars};

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

      dag<Type> pg_g {pg_edges.dag, pg_dists.dag_dist};

      persistent_graph<Type> pg {pg_re, pg_re, pg_g};

      vector<int> idx(3);
      idx << 0,2,4;
      array<Type> small_s_re = pg.subset_re_by_s(idx);
      REPORT(small_s_re);
      array<Type> small_s_mean = pg.subset_mean_by_s(idx);
      REPORT(small_s_mean);

      re_dag_node<Type> pgnode = pg(1);
      array<Type> small_g_re = pgnode.re;
      array<Type> small_g_mean = pgnode.mean;
      matrix<Type> small_g_di = pgnode.node.d;
      REPORT(small_g_re);
      REPORT(small_g_mean);
      REPORT(small_g_di);

      re_dag_node<Type> one_node = pg(1,0,0);
      array<Type> one_g_re = one_node.re;
      array<Type> one_g_mean = one_node.mean;
      REPORT(one_g_re);
      REPORT(one_g_mean);

      array<Type> small_t_re = pg.slice_t(1,2).re;
      array<Type> small_t_mean = pg.slice_t(1,2).mean;
      REPORT(small_t_re);
      REPORT(small_t_mean);

      array<Type> small_v_re = pg.slice_v(1,1).re;
      array<Type> small_v_mean = pg.slice_v(1,1).mean;
      REPORT(small_v_re);
      REPORT(small_v_mean);


      vector<Type> new_vals(pg(0).node.to.size());
      for(int i=0; i<new_vals.size(); i++) {
        new_vals(i) = -1.0*Type(i+1)/2.0;
      }
      array<Type> overwrite_re = pg.set_re_by_to_g(new_vals,0,0,0).re;
      REPORT(overwrite_re);
      array<Type> after_overwrite_re = pg.re;
      REPORT(after_overwrite_re);

      vector<Type> overwrite_re1 = pg(0,0,0).re;
      REPORT(overwrite_re1);

      for(int i=0; i<new_vals.size(); i++) {
        new_vals(i) = -20.0-Type(i);
      }
      array<Type> overwrite_mean = pg.set_mean_by_to_g(new_vals,0,0,0).mean;
      REPORT(overwrite_mean);
      array<Type> after_overwrite_mean = pg.mean;
      REPORT(after_overwrite_mean);

      vector<Type> overwrite_mean1 = pg(0,0,0).mean;
      REPORT(overwrite_mean1);






    } else if(test == "transient_graph" ) {
      DATA_ARRAY(pg_re); // [space,time,var]
      DATA_STRUCT(pg_edges,directed_graph);
      DATA_STRUCT(pg_dists,dag_dists);
      dag<Type> pg_g(pg_edges.dag,pg_dists.dag_dist);

      persistent_graph<Type> pg {pg_re ,pg_re ,pg_g};

      DATA_ARRAY(tg_re);
      DATA_STRUCT(tg_edges,directed_graph);
      DATA_STRUCT(tg_dists,dag_dists);
      dag<Type> tg_g{tg_edges.dag, tg_dists.dag_dist};

      DATA_IVECTOR(t);

      transient_graph<Type> tg {
        tg_re,
        tg_re,
        tg_g,
        t,
        pg.dim_t() // # of times
      };

      vector<int> dim_g(tg.dim_t());
      for(int t=0; t<tg.dim_t(); t++) {
        dim_g(t) = tg.dim_g(t);
      }
      vector<int> dim_s(tg.dim_t());
      for(int t=0; t<tg.dim_t(); t++) {
        dim_s(t) = tg.dim_s(t);
      }
      int dim_t = tg.dim_t();
      int dim_v = tg.dim_v();
      REPORT(dim_g);
      REPORT(dim_s);
      REPORT(dim_t);
      REPORT(dim_v);

      array<Type> full_re = tg.get_re();
      REPORT(full_re);
      array<Type> full_mean = tg.get_mean();
      REPORT(full_mean);
      vector<dag<Type> > full_graph = tg.get_graph();

      re_dag_node<Type> tg_node = tg(2,0,pg);
      array<Type> small_tg_re = tg_node.re;
      array<Type> small_tg_mean = tg_node.mean;
      matrix<Type> small_tg_di = tg_node.node.d;
      REPORT(small_tg_re);
      REPORT(small_tg_mean);
      REPORT(small_tg_di);

      re_dag_node<Type> stg_node = tg(2,0,0,pg);
      array<Type> ssmall_tg_re = stg_node.re;
      array<Type> ssmall_tg_mean = stg_node.mean;
      matrix<Type> ssmall_tg_di = stg_node.node.d;
      REPORT(ssmall_tg_re);
      REPORT(ssmall_tg_mean);
      REPORT(ssmall_tg_di);



      array<Type> small_t_re = tg.slice_t(0,1).get_re();
      REPORT(small_t_re);
      array<Type> small_t_mean = tg.slice_t(0,1).get_mean();
      REPORT(small_t_mean);

      array<Type> small_v_re = tg.slice_v(1,1).get_re();
      REPORT(small_v_re);
      array<Type> small_v_mean = tg.slice_v(1,1).get_mean();
      REPORT(small_v_mean);



      vector<Type> new_vals(tg(0,0,pg).node.to.size());
      for(int i=0; i<new_vals.size(); i++) {
        new_vals(i) = -1.0*Type(i+1)/2.0;
      }
      array<Type> overwrite_re = tg.set_re_by_to_g(new_vals,0,0,0,pg).get_re();
      REPORT(overwrite_re);
      array<Type> after_overwrite_re = tg.get_re();
      REPORT(after_overwrite_re);

      for(int i=0; i<new_vals.size(); i++) {
        new_vals(i) = -20.0-Type(i);
      }
      array<Type> overwrite_mean = tg.set_mean_by_to_g(new_vals,0,0,0,pg).get_mean();
      REPORT(overwrite_mean);
      array<Type> after_overwrite_mean = tg.get_mean();
      REPORT(after_overwrite_mean);

      vector<Type> overwrite_mean1 = tg(0,0,0,pg).mean;
      REPORT(overwrite_mean1);





    } else if(test == "nngp") {
      DATA_ARRAY(ts_re);
      DATA_ARRAY(ts_pars);

      time_series<Type> ts {ts_re,ts_pars};

      DATA_ARRAY(pg_re); // [space,time,var]
      DATA_STRUCT(pg_edges,directed_graph);
      DATA_STRUCT(pg_dists,dag_dists);
      dag<Type> pg_g(pg_edges.dag,pg_dists.dag_dist);

      persistent_graph<Type> pg {pg_re,pg_re,pg_g};

      DATA_ARRAY(tg_re);
      DATA_STRUCT(tg_edges,directed_graph);
      DATA_STRUCT(tg_dists,dag_dists);
      dag<Type> tg_g(tg_edges.dag,tg_dists.dag_dist);

      DATA_IVECTOR(t);

      transient_graph<Type> tg {tg_re,tg_re,tg_g,t,pg.dim_t()};

      DATA_ARRAY(cv_pars);
      DATA_IVECTOR(cv_code);

      vector<covariance<Type> > cv(pg.dim_v());
      for(int v=0; v<pg.dim_v(); v++) {
        cv(v) = covariance<Type>(vector<Type>(cv_pars.col(v)),cv_code(v));
      }

      nngp<Type> process {pg,tg,cv};

      array<Type> bnngp_pg_re = process.get_pg_re();
      REPORT(bnngp_pg_re);
      array<Type> bnngp_pg_mean = process.get_pg_mean();
      REPORT(bnngp_pg_mean);


      Type one_pg_re = process(1,0,0);
      REPORT(one_pg_re);

      Type one_tg_re = process(pg.dim_s()+2,0,1);
      REPORT(one_tg_re);


      Type nngp2_ll = process.loglikelihood(ts);
      REPORT(nngp2_ll);

      array<Type> nngp_pg_re = process.get_pg_re();
      REPORT(nngp_pg_re);
      array<Type> nngp_pg_mean = process.get_pg_mean();
      REPORT(nngp_pg_mean);

      array<Type> ts_sim = ts.simulate().get_re();
      REPORT(ts_sim);
      array<Type> sim_nngp_pg_re = process.simulate(ts).get_pg_re();
      REPORT(sim_nngp_pg_re);
      array<Type> sim_nngp_tg_re = process.get_tg_re();
      REPORT(sim_nngp_tg_re);

      Type nngp2_sim_ll = process.loglikelihood(ts);
      REPORT(nngp2_sim_ll);






    } else if(test == "inv_link_function") {
      DATA_VECTOR(x);

      array<Type> ans(x.size(),3);
      for(int code=0; code<3; code++) { // code < # of cases in inv_link_function
        for(int i=0; i<x.size(); i++) {
          ans(i,code) = inv_link_function{code}(x(i));
        }
      }
      REPORT(ans);





    } else if(test == "distribution") {
      DATA_MATRIX(x);
      DATA_VECTOR(mean);
      DATA_MATRIX(pars);
      DATA_MATRIX(size);

      matrix<Type> ans = x;
      for(int code=0; code<10; code++) { // code < # of cases in distribution
        for(int i=0; i<x.rows(); i++) {
          ans(i,code) = distribution<Type>{code,pars.col(code)}(x(i,code),mean(code),size(i,code));
        }
      }
      REPORT(ans);






    } else if(test == "family") {
      // No tests needed
      DATA_MATRIX(x);
      DATA_VECTOR(mean);
      DATA_MATRIX(pars);
      DATA_MATRIX(size);

      matrix<Type> ans = x;
      for(int code=0; code<10; code++) {
        int link_code=0;
        switch(code) {
          case 0 : link_code=0; break; // Normal - identity
          case 1 : link_code=1; break; // Poisson - log
          case 2 : link_code=1; break; // Neg. Binom - log
          case 3 : link_code=2; break; // Bernoulli - logit
          case 4 : link_code=1; break; // Gamma - log
          case 5 : link_code=0; break; // Log-normal - identity
          case 6 : link_code=2; break; // Binomial - logit
          case 7 : link_code=2; break; // atLeastOneBinomial - logit
          case 8 : link_code=1; break; // Compois - log
          case 9 : link_code=1; break; // Tweedie - log
        };
        for(int i=0; i<x.rows(); i++) {
          ans(i,code) = family<Type>{
            {link_code},
            {code,pars.col(code)}
          }(x(i,code),mean(code),size(i,code));
        }
      }
      REPORT(ans);








    } else if(test == "observations") {
      DATA_ARRAY(ts_re);
      DATA_ARRAY(ts_pars);

      time_series<Type> ts {ts_re,ts_pars};

      DATA_ARRAY(pg_re); // [space,time,var]
      DATA_STRUCT(pg_edges,directed_graph);
      DATA_STRUCT(pg_dists,dag_dists);
      dag<Type> pg_g(pg_edges.dag,pg_dists.dag_dist);

      persistent_graph<Type> pg {pg_re,pg_re,pg_g};

      DATA_ARRAY(tg_re);
      DATA_STRUCT(tg_edges,directed_graph);
      DATA_STRUCT(tg_dists,dag_dists);
      dag<Type> tg_g(tg_edges.dag,tg_dists.dag_dist);

      DATA_IVECTOR(t);

      transient_graph<Type> tg {tg_re,tg_re,tg_g,t,pg.dim_t()};

      DATA_ARRAY(cv_pars);
      DATA_IVECTOR(cv_code);

      vector<covariance<Type> > cv(pg.dim_v());
      for(int v=0; v<pg.dim_v(); v++) {
        cv(v) = covariance<Type>(vector<Type>(cv_pars.col(v)),cv_code(v));
      }

      nngp<Type> process {pg,tg,cv};


      DATA_ARRAY(obs);
      DATA_IARRAY(idx);
      DATA_ARRAY(sample_size);
      DATA_MATRIX(mean_design);
      DATA_MATRIX(beta);
      DATA_IARRAY(family_codes);
      DATA_ARRAY(family_pars);

      vector<family<Type> > families(family_codes.cols());
      for(int v=0; v<family_codes.cols(); v++) {
        families(v) = family<Type> {
          {family_codes(0,v)},
          {family_codes(1,v), family_pars.col(v)}
        };
      }

      observations<Type> glmm {obs,idx,sample_size,mean_design,beta,families};

      array<Type> mm = glmm.get_link_mean(process);
      REPORT(mm);

      Type ll = glmm.loglikelihood(process);
      REPORT(ll);

      ts.simulate();
      process.simulate(ts);
      glmm.simulate(process);

      array<Type> new_ts = ts.get_re();
      REPORT(new_ts);

      array<Type> new_pg = process.get_pg_re();
      REPORT(new_pg);
      array<Type> new_tg = process.get_tg_re();
      REPORT(new_tg);

      array<Type> new_obs = glmm.obs;
      REPORT(new_obs);


      array<Type> new_mm = glmm.get_link_mean(process);
      REPORT(new_mm);

      Type new_ll = glmm.loglikelihood(process);
      REPORT(new_ll);

    } else {}


    Type nll = -1.0*pow(dummy,2);
    return nll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
