// Headers are included in staRVe.cpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template<class Type>
Type staRVe_model(objective_function<Type>* obj) {
  // Read in data / parameters / random effects from R
  DATA_IVECTOR(distribution_code); // [var]
  DATA_IVECTOR(link_code); // [var]

  DATA_IVECTOR(y_time);
  DATA_MATRIX(obs_y); // [idx,var]
  DATA_STRUCT(ys_edges,directed_graph); // See data_in.hpp
  DATA_STRUCT(ys_dists,dag_dists); // See data_in.hpp

  DATA_IVECTOR(resp_w_time);

  DATA_MATRIX(mean_design);
  DATA_MATRIX(sample_size); // [idx,var]

  DATA_IVECTOR(covar_code); // [var]
  DATA_STRUCT(ws_edges,directed_graph);
  DATA_STRUCT(ws_dists,dag_dists);

  DATA_IVECTOR(pred_w_time);
  DATA_STRUCT(pred_ws_edges,directed_graph); // See data_in.hpp
  DATA_STRUCT(pred_ws_dists,dag_dists); // See data_in.hpp

  DATA_MATRIX(copula_weights); // [space,time]

  DATA_INTEGER(conditional_sim); // If true, don't simulate w

  PARAMETER_MATRIX(working_response_pars); // Response distribution parameters [par,var]
  // Columns may have trailing NAs if distribution haven't different # of parameters
  PARAMETER_MATRIX(mean_pars); // Fixed effects B*X [covariate,var]
  PARAMETER_MATRIX(resp_w); // [idx,var]
  PARAMETER_MATRIX(working_space_pars); // [par,var]
  PARAMETER_MATRIX(time_effects); // [time,var]
  PARAMETER_MATRIX(working_time_pars); // [par,var]
  PARAMETER_ARRAY(proc_w); // [space,time,var]
  PARAMETER_MATRIX(pred_w); // [idx,var]
  PARAMETER_VECTOR(logit_copula_cor);


  int ns=proc_w.col(0).col(0).cols();
  int nt=proc_w.col(0).cols();
  int nv=proc_w.cols();

  // Get graphs

  // Transient graph
  vector<vector<vector<int> > > ys_dag = ys_edges.dag;
  vector<matrix<Type> > ys_dist = ys_dists.dag_dist;

  // Persistent graph
  vector<vector<vector<int> > > ws_dag = ws_edges.dag;
  vector<matrix<Type> > ws_dist = ws_dists.dag_dist;

  // Graph for predictions
  vector<vector<vector<int> > > pred_ws_dag = pred_ws_edges.dag;
  vector<matrix<Type> > pred_ws_dist = pred_ws_dists.dag_dist;

  Type mean_dist = 0.0;
  Type n = 0.0;
  for( int i=0; i<ws_dist.size(); i++ ) {
    mean_dist += ws_dist(i).sum();
    n += ws_dist(i).size();
  }
  mean_dist *= 1/n;
  for( int i=0; i<ys_dist.size(); i++ ) {
    ys_dist(i) = ys_dist(i)/mean_dist;
  }
  for( int i=0; i<ws_dist.size(); i++ ) {
    ws_dist(i) = ws_dist(i)/mean_dist;
  }
  for( int i=0; i<pred_ws_dist.size(); i++ ) {
    pred_ws_dist(i) = pred_ws_dist(i)/mean_dist;
  }


  // Convert parameters from working scale to natural scale

  // Cross-check distribution_code order with family.hpp
  matrix<Type> response_pars = working_response_pars;
  for(int v=0; v<nv; v++) {
    switch(distribution_code(v)) {
      case 0 : response_pars(0,v) = exp(working_response_pars(0,v)); break; // Normal, sd>0
      case 1 : break; // Poisson, NA
      case 2 : response_pars(0,v) = exp(working_response_pars(0,v))+1; break; // Neg. Binom., overdispersion > 1
      case 3 : break; // Bernoulli, NA
      case 4 : response_pars(0,v) = exp(working_response_pars(0,v)); break; // Gamma, sd>0
      case 5 : response_pars(0,v) = exp(working_response_pars(0,v)); break; // Log-Normal, sd>0
      case 6 : break; // Binomial, NA
      case 7 : break; // atLeastOneBinomial, NA
      case 8 : response_pars(0,v) = exp(working_response_pars(0,v)); break; // Conway-Maxwell-Poisson, dispersion > 0
      case 9 : response_pars(0,v) = exp(working_response_pars(0,v)); // Tweedie, scale > 0
               // response_pars(1) = plogis(working_response_pars(0))+1; break;// 1 < power < 2
               response_pars(1,v) = 1.0/(1.0+exp(-working_response_pars(1,v)))+1.0; break;
      default : response_pars(0,v) = exp(-1*working_response_pars(0,v)); break; // Normal, sd>0
    }
  }
  matrix<Type> space_pars = working_space_pars;
  for(int v=0; v<nv; v++) {
    switch(covar_code(v)) {
      default : space_pars.col(v) = exp(vector<Type>(working_space_pars.col(v))); break; // Matern-types
    }
    // standardizes range, which then standardizes sd
    space_pars(1,v) = space_pars(1,v)/mean_dist;
  }

  matrix<Type> time_pars = working_time_pars;
  for(int v=0; v<nv; v++) {
    time_pars(0,v) = working_time_pars(0,v); // mu
    time_pars(1,v) = 2*invlogit(working_time_pars(1,v))-1; // -1 < ar1 < +1
    time_pars(2,v) = exp(working_time_pars(2,v)); // sd>0
  }


  vector<Type> copula_cor = 2*invlogit(logit_copula_cor)-1;
  matrix<Type> R(nv,nv);
  R.setIdentity();
  if( copula_cor.size() == 1 ) {
    R(1,0) = copula_cor(0);
    R(0,1) = copula_cor(0);
  }
  mvn_copula<Type> copula(R);

  array<Type> standard_w(ns,nt,nv);
  array<Type> standard_resp_w(resp_w.rows(),nv);
  array<Type> t_standard_w(nv,nt,ns);
  array<Type> t_standard_resp_w(nv,resp_w.rows());

  SIMULATE{
    for(int s=0; s<ns; s++) {
      for(int t=0; t<nt; t++) {
        t_standard_w.col(s).col(t) = copula.simulate();
      }
    }

    for(int i=0; i<t_standard_resp_w.cols(); i++) {
      t_standard_resp_w.col(i) = copula.simulate();
    }

    standard_w = t_standard_w.transpose();
    standard_resp_w = t_standard_resp_w.transpose();
  }

  matrix<Type> standard_pred_w = pred_w;
  matrix<Type> t_standard_pred_w = standard_pred_w.transpose();

  // Initialize all the objects used. The main objects of focus are
  // nngp<Type> process which can calculate the nll component for the random effects, and
  // observations<Type> obs which can calculate the nll component for the observations

  // Initialize vectors used to slice out individual times for each of the
  // random effect / data streams. First element will give the index for the
  // first random effect for each time, second element will give the number of
  // random effects to take out.
  vector<int> y_segment(2);
  vector<int> resp_w_segment(2);
  vector<int> pred_w_segment(2);

  // Initializes (joint) negative log-likelihood to 0
  Type nll = 0.0;
  Type pred_nll = 0.0;

  // AR1 process for time effects
  if( time_effects.rows() == 1 ) {
    // If there's only one time, force temporal random effect to be approx. equal
    // to mu
    Type smallNumber = pow(10,-5);
    for(int v=0; v<nv; v++) {
      nll -= dnorm(time_effects(0,v),time_pars(0,v),smallNumber,true);
      SIMULATE{
        if( !conditional_sim ) {
          time_effects(0,v) = time_pars(0,v);
        } else {}
      }
    }
  } else {
    // More than one time, use AR1 covariance structure
    for(int v=0; v<nv; v++) {
      matrix<Type> time_cov(time_effects.rows(),time_effects.rows());
      for(int i=0; i<time_cov.rows(); i++) {
        for(int j=0; j<time_cov.cols(); j++) {
          time_cov(i,j) = pow(time_pars(1,v),abs(i-j));
        }
      }
      time_cov *= pow(time_pars(2,v),2);
      // time_cov *= pow(time_sd,2)/(1-pow(time_ar1,2)); // time_sd gives sd of
      // innovations, this coefficient is the marginal variance
      MVNORM_t<Type> time_dist(time_cov);

      nll += time_dist(vector<Type>(time_effects.col(v))-time_pars(0,v));
      SIMULATE{
        if( !conditional_sim ) {
          vector<Type> sim_time_effects(time_effects.rows());
          time_dist.simulate(sim_time_effects);
          time_effects.col(v) = sim_time_effects+time_pars(0,v);
        } else {}
      }
    }
  }

  // Likelihood contributions for spatio-temporal random effects and observations
  for(int v=0; v<nv; v++) {
    // Initial time segments
    y_segment = get_time_segment(y_time,0);
    resp_w_segment = get_time_segment(resp_w_time,0);
    pred_w_segment = get_time_segment(pred_w_time,0);

    // cov = covariance function
    // proc_w = spatio-temporal random effects for this time
    // ws_dag = edge list for persistent graph
    // ws_dist = distances for persistent graph
    nngp<Type> process(covariance<Type>(space_pars(0,v),space_pars(1,v),space_pars(2,v),covar_code(v)),
                       proc_w.col(v).col(0),
                       time_effects(0,v)+0*proc_w.col(v).col(0),
                       // ^ gives constant mean
                       ws_dag,
                       ws_dist);

    // process = spatio-temporal random effects, and related utilities
    // obs_y = response data
    // ys_dag = edge list for transient graph
    // ys_dist = distances for transient graph
    // resp_w = additional spatio-temporal random effects needed for transient graph
    // mean_design = covariate data
    // sample.size = sample size info for binomial distribution
    // family = response distribution and link function
    // Set up the response distribution and link function
    observations<Type> obs(process,
                           obs_y.col(v).segment(y_segment(0),y_segment(1)),
                           ys_dag.segment(y_segment(0),y_segment(1)),
                           ys_dist.segment(y_segment(0),y_segment(1)),
                           resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1)),
                           matrix_row_segment(mean_design,y_segment(0),y_segment(1)),
                           sample_size.col(v).segment(y_segment(0),y_segment(1)),
                           glm<Type>({link_code(v)},
                                     {distribution_code(v)},
                                     mean_pars.col(v),
                                     vector<Type>(response_pars.col(v)))
                          );

    // pred_ws_dag = edge list for predictions
    // pred_ws_dist = distances for predictions
    // pred_w = spatio-temporal random effects for predictions
    // this_std_pred_w = vector to hold standard version of pred_w, passed by reference
    // pred_nll = likelihood component for predictions, passed by reference
    //   so it's updated by calling predict_w
    bool have_set_pred_cache = false;
    if( pred_w_segment(1) > 0 ) {
      vector<Type> this_pred_w = pred_w.col(v).segment(pred_w_segment(0),pred_w_segment(1));
      process.predict_w(pred_ws_dag,
                        pred_ws_dist,
                        pred_w.col(v).segment(pred_w_segment(0),pred_w_segment(1)),
                        this_pred_w,
                        pred_nll,
                        have_set_pred_cache, // Don't use cache (doesn't exist yet)
                        not have_set_pred_cache); // Write the cache
      // have_set_pred_cache = true;
      standard_pred_w.col(v).segment(pred_w_segment(0),pred_w_segment(1)) = this_pred_w;
    } else {}

    // Add likelihood components to joint likelihood
    nll -= process.loglikelihood()
              + obs.resp_w_loglikelihood()
              + obs.y_loglikelihood();

    SIMULATE{
      if( !conditional_sim ) {
        // Simulate new random effects, if desired
        proc_w.col(v).col(0) = process.simulate(standard_w.col(v).col(0));
        resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1)) = obs.simulate_resp_w(
          standard_resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1))
        );
      } else {}
      // Simulate new response data
      obs_y.col(v).segment(y_segment(0),y_segment(1)) = obs.simulate_y();
    }

    // Update standard_w AFTER simulations with proc_w. Otherwise the random effects
    // won't be simulated correctly
    standard_w.col(v).col(0) = process.get_standard_w();
    standard_resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1)) = obs.get_standard_resp_w();

    // Update process and observations for each time step,
    // add their likelihood contributions
    for(int time=1; time<time_effects.rows(); time++) {
      // Get indices for this time
      y_segment = get_time_segment(y_time,time);
      resp_w_segment = get_time_segment(resp_w_time,time);
      pred_w_segment = get_time_segment(pred_w_time,time);

      // Update the random effects and the mean function
      // the mean function here ensures a marginal AR(1) process at each location
      process.update_w(proc_w.col(v).col(time),
                       time_effects(time,v) + time_pars(1)*(process.get_w()-time_effects(time-1,v)));
      // Update the data, covariates, transient graph, and extra random effects
      obs.update_y(obs_y.col(v).segment(y_segment(0),y_segment(1)),
                   ys_dag.segment(y_segment(0),y_segment(1)),
                   ys_dist.segment(y_segment(0),y_segment(1)),
                   resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1)),
                   matrix_row_segment(mean_design,y_segment(0),y_segment(1)),
                   sample_size.col(v).segment(y_segment(0),y_segment(1)));

      // Likelihood component for predictions
      if( pred_w_segment(1) > 0 ) {
      vector<Type> this_pred_w = pred_w.col(v).segment(pred_w_segment(0),pred_w_segment(1));
      process.predict_w(pred_ws_dag,
                        pred_ws_dist,
                        pred_w.col(v).segment(pred_w_segment(0),pred_w_segment(1)),
                        this_pred_w,
                        pred_nll,
                        have_set_pred_cache, // Use cache
                        not have_set_pred_cache); // Don't overwrite cache
        // have_set_pred_cache = true;
        standard_pred_w.col(v).segment(pred_w_segment(0),pred_w_segment(1)) = this_pred_w;
      } else {}

      nll -= process.loglikelihood()
                + obs.resp_w_loglikelihood()
                + obs.y_loglikelihood();


      SIMULATE{
        if( !conditional_sim ) {
          // Simulate new random effecst
          proc_w.col(v).col(time) = process.simulate(standard_w.col(v).col(time));
          resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1)) = obs.simulate_resp_w(
            standard_resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1))
          );
        } else {}
        // Simulate new response data
        obs_y.col(v).segment(y_segment(0),y_segment(1)) = obs.simulate_y();
      }

      // Update standard_w AFTER simulations with proc_w. Otherwise the random effects
      // won't be simulated correctly
      standard_w.col(v).col(time) = process.get_standard_w();
      standard_resp_w.col(v).segment(resp_w_segment(0),resp_w_segment(1)) = obs.get_standard_resp_w();
    }
  }

  t_standard_w = standard_w.transpose(); // [var,time,space]
  t_standard_resp_w = standard_resp_w.transpose(); // [var,idx]
  t_standard_pred_w = standard_pred_w.transpose();

  for(int s=0; s<ns; s++) {
    for(int t=0; t<nt; t++) {
      vector<Type> wrow = t_standard_w.col(s).col(t);
      nll += copula_weights(s,t)*copula(wrow);
    }
  }

  // NOTE: Still need to weight this if I keep the weights
  for(int i=0; i<t_standard_resp_w.cols(); i++) {
    vector<Type> wrow = t_standard_resp_w.col(i);
    nll += copula(wrow);
  }

  for(int i=0; i<t_standard_pred_w.cols(); i++) {
    vector<Type> wrow = t_standard_pred_w.col(i);
    nll += copula(wrow);
  }

  // Report back the simulated random effects and data to R
  SIMULATE{
    REPORT(time_effects);
    REPORT(t_standard_w.transpose());
    REPORT(proc_w);
    REPORT(resp_w);
    REPORT(obs_y);
  }

  REPORT(standard_w);

  REPORT(response_pars);
  ADREPORT(response_pars);

  for(int v=0; v<nv; v++) {
    // Spatial range back to natural scale
    space_pars(1,v) = space_pars(1,v)*mean_dist;
  }
  REPORT(space_pars);
  ADREPORT(space_pars);

  REPORT(time_pars);
  ADREPORT(time_pars);

  REPORT(copula_cor);
  ADREPORT(copula_cor);

  return(nll+pred_nll);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
