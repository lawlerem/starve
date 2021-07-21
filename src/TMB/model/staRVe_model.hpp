// Headers are included in staRVe.cpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template<class Type>
Type staRVe_model(objective_function<Type>* obj) {
  // Read in data / parameters / random effects from R
  DATA_INTEGER(n_time);
  DATA_INTEGER(distribution_code);
  DATA_INTEGER(link_code);

  DATA_IVECTOR(y_time);
  DATA_VECTOR(obs_y);
  DATA_VECTOR_INDICATOR(keep,obs_y); // Sets up TMB residuals if you want them
  DATA_STRUCT(ys_edges,directed_graph); // See data_in.hpp
  DATA_STRUCT(ys_dists,dag_dists); // See data_in.hpp

  DATA_MATRIX(resp_w_mean_design);
  DATA_IVECTOR(resp_w_time);

  DATA_MATRIX(mean_design);
  DATA_IVECTOR(sample_size);

  DATA_MATRIX(w_mean_design);
  DATA_IVECTOR(w_mean_pars_idx); // Want to use the same fixed effects as y
  DATA_INTEGER(covar_code);
  DATA_IVECTOR(w_time);
  DATA_STRUCT(ws_edges,directed_graph);
  DATA_STRUCT(ws_dists,dag_dists);

  DATA_MATRIX(pred_w_mean_design);
  DATA_IVECTOR(pred_w_time);
  DATA_STRUCT(pred_ws_edges,directed_graph); // See data_in.hpp
  DATA_STRUCT(pred_ws_dists,dag_dists); // See data_in.hpp

  DATA_INTEGER(conditional_sim); // If true, don't simulate w

  PARAMETER_VECTOR(working_response_pars); // Response distribution parameters except for mean
  PARAMETER_VECTOR(mean_pars); // Fixed effects B*X
  PARAMETER_VECTOR(resp_w);
  PARAMETER(log_space_sd);
  PARAMETER(log_space_rho);
  PARAMETER(log_space_nu);
  PARAMETER_VECTOR(time_effects);
  PARAMETER(time_mu);
  PARAMETER(logit_time_ar1);
  PARAMETER(log_time_sd);
  PARAMETER_VECTOR(proc_w);
  PARAMETER_VECTOR(pred_w);


  // Convert parameters from working scale to natural scale

  // Cross-check distribution_code order with family.hpp
  vector<Type> response_pars = working_response_pars;
  switch(distribution_code) {
    case 0 : response_pars(0) = exp(working_response_pars(0)); break; // Normal, sd>0
    case 1 : break; // Poisson, NA
    case 2 : response_pars(0) = exp(working_response_pars(0))+1; break; // Neg. Binom., overdispersion > 1
    case 3 : break; // Bernoulli, NA
    case 4 : response_pars(0) = exp(working_response_pars(0)); break; // Gamma, sd>0
    case 5 : response_pars(0) = exp(working_response_pars(0)); break; // Log-Normal, sd>0
    case 6 : break; // Binomial, NA
    case 7 : break; // atLeastOneBinomial, NA
    case 8 : response_pars(0) = exp(working_response_pars(0)); break; // Conway-Maxwell-Poisson, dispersion > 0
    default : response_pars(0) = exp(-1*working_response_pars(0)); break; // Normal, sd>0
  }
  Type space_sd = exp(log_space_sd); // sd>0
  Type space_rho = exp(log_space_rho); // rho>0
  Type space_nu = exp(log_space_nu); // nu>0

  Type time_ar1 = 2*invlogit(logit_time_ar1)-1; // -1 < ar1 < +1
  Type time_sd = exp(log_time_sd); // sd>0

  // Subtract off fixed effects to alleviate spatial confounding
  vector<Type> w_mean_pars = mean_pars(w_mean_pars_idx);
  proc_w -= w_mean_design*w_mean_pars;
  resp_w -= resp_w_mean_design*w_mean_pars;
  pred_w -= pred_w_mean_design*w_mean_pars;

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

  // Standardize distances (based on persistent graph) so rho doesn't scale with distance
  // i.e. distance units don't matter
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
  space_rho = space_rho/mean_dist;

  // Initialize all the objects used. The main objects of focus are
  // nngp<Type> process which can calculate the nll component for the random effects, and
  // observations<Type> obs which can calculate the nll component for the observations
  covariance<Type> cov(space_sd,space_rho,space_nu,covar_code);

  // Set up the response distribution and link function
  inv_link_function inv_link = {link_code};
  response_density y_density = {distribution_code};
  glm<Type> family(inv_link,
                   y_density,
                   mean_pars,
                   response_pars);

  // Initialize vectors used to slice out individual times for each of the
  // random effect / data streams. First element will give the index for the
  // first random effect for each time, second element will give the number of
  // random effects to take out.
  vector<int> w_segment(2);
  vector<int> y_segment(2);
  vector<int> resp_w_segment(2);
  vector<int> pred_w_segment(2);

  // Initializes (joint) negative log-likelihood to 0
  Type nll = 0.0;
  Type pred_nll = 0.0;

  // AR1 process for time effects
  if( time_effects.size() == 1 ) {
    // If there's only one time, force temporal random effect to be approx. equal
    // to mu
    Type smallNumber = pow(10,-5);

    nll -= dnorm(time_effects(0),time_mu,smallNumber,true);
    SIMULATE{
      if( !conditional_sim ) {
        time_effects(0) = time_mu;
      } else {}
    }
  } else {
    // More than one time, use AR1 covariance structure
    matrix<Type> time_cov(time_effects.size(),time_effects.size());
    for(int i=0; i<time_cov.rows(); i++) {
      for(int j=0; j<time_cov.cols(); j++) {
        time_cov(i,j) = pow(time_ar1,abs(i-j));
      }
    }
    time_cov *= pow(time_sd,2);
    // time_cov *= pow(time_sd,2)/(1-pow(time_ar1,2)); // time_sd gives sd of
    // innovations, this coefficient is the marginal variance
    MVNORM_t<Type> time_dist(time_cov);

    nll += time_dist(time_effects-time_mu);
    SIMULATE{
      if( !conditional_sim ) {
        time_dist.simulate(time_effects);
        time_effects = time_effects+time_mu;
      } else {}
    }
  }

  // Likelihood contributions for spatio-temporal random effects and observations

  // Initial time segments
  w_segment = get_time_segment(w_time,0);
  y_segment = get_time_segment(y_time,0);
  resp_w_segment = get_time_segment(resp_w_time,0);
  pred_w_segment = get_time_segment(pred_w_time,0);


  // cov = covariance function
  // proc_w = spatio-temporal random effects for this time
  // ws_dag = edge list for persistent graph
  // ws_dist = distances for persistent graph
  nngp<Type> process(cov,
                     proc_w.segment(w_segment(0),w_segment(1)),
                     time_effects(0)+0*proc_w.segment(w_segment(0),w_segment(1)),
                     // ^ gives constant mean
                     ws_dag,
                     ws_dist);

  // process = spatio-temporal random effects, and related utilities
  // obs_y = response data
  // keep = used for TMB residuals if wanted
  // ys_dag = edge list for transient graph
  // ys_dist = distances for transient graph
  // resp_w = additional spatio-temporal random effects needed for transient graph
  // mean_design = covariate data
  // sample.size = sample size info for binomial distribution
  // family = response distribution and link function
  observations<Type> obs(process,
                         obs_y.segment(y_segment(0),y_segment(1)),
                         keep.segment(y_segment(0),y_segment(1)),
                         ys_dag.segment(y_segment(0),y_segment(1)),
                         ys_dist.segment(y_segment(0),y_segment(1)),
                         resp_w.segment(resp_w_segment(0),resp_w_segment(1)),
                         matrix_row_segment(mean_design,y_segment(0),y_segment(1)),
                         sample_size.segment(y_segment(0),y_segment(1)),
                         family);

  // pred_ws_dag = edge list for predictions
  // pred_ws_dist = distances for predictions
  // pred_w = spatio-temporal random effects for predictions
  // pred_nll = likelihood component for predictions, passed by reference
  //   so it's updated by calling predict_w
  bool have_set_pred_cache = false;
  if( pred_w_segment(1) > 0 ) {
    process.predict_w(pred_ws_dag,
                      pred_ws_dist,
                      pred_w.segment(pred_w_segment(0),pred_w_segment(1)),
                      pred_nll,
                      have_set_pred_cache, // Don't use cache (doesn't exist yet)
                      not have_set_pred_cache); // Write the cache
    // have_set_pred_cache = true;
  } else {}

  // Add likelihood components to joint likelihood
  nll -= process.loglikelihood()
            + obs.resp_w_loglikelihood()
            + obs.y_loglikelihood();
  SIMULATE{
    if( !conditional_sim ) {
      // Simulate new random effects, if desired
      proc_w.segment(w_segment(0),w_segment(1)) = process.simulate();
      resp_w.segment(resp_w_segment(0),resp_w_segment(1)) = obs.simulate_resp_w();
    } else {}
    // Simulate new response data
    obs_y.segment(y_segment(0),y_segment(1)) = obs.simulate_y();
  }


  // Update process and observations for each time step,
  // add their likelihood contributions
  for(int time=1; time<n_time; time++) {
    // Get indices for this time
    w_segment = get_time_segment(w_time,time);
    y_segment = get_time_segment(y_time,time);
    resp_w_segment = get_time_segment(resp_w_time,time);
    pred_w_segment = get_time_segment(pred_w_time,time);

    // Update the random effects and the mean function
    // the mean function here ensures a marginal AR(1) process at each location
    process.update_w(proc_w.segment(w_segment(0),w_segment(1)),
                     time_effects(time) + time_ar1*(process.get_w()-time_effects(time-1)));
    // Update the data, covariates, transient graph, and extra random effects
    obs.update_y(obs_y.segment(y_segment(0),y_segment(1)),
                 keep.segment(y_segment(0),y_segment(1)),
                 ys_dag.segment(y_segment(0),y_segment(1)),
                 ys_dist.segment(y_segment(0),y_segment(1)),
                 resp_w.segment(resp_w_segment(0),resp_w_segment(1)),
                 matrix_row_segment(mean_design,y_segment(0),y_segment(1)),
                 sample_size.segment(y_segment(0),y_segment(1)));

    // Likelihood component for predictions
    if( pred_w_segment(1) > 0 ) {
    process.predict_w(pred_ws_dag,
                      pred_ws_dist,
                      pred_w.segment(pred_w_segment(0),pred_w_segment(1)),
                      pred_nll,
                      have_set_pred_cache, // Use cache
                      not have_set_pred_cache); // Don't overwrite cache
      // have_set_pred_cache = true;
    } else {}

    nll -= process.loglikelihood()
              + obs.resp_w_loglikelihood()
              + obs.y_loglikelihood();

    SIMULATE{
      if( !conditional_sim ) {
        // Simulate new random effecst
        proc_w.segment(w_segment(0),w_segment(1)) = process.simulate();
        resp_w.segment(resp_w_segment(0),resp_w_segment(1)) = obs.simulate_resp_w();
      } else {}
      // Simulate new response data
      obs_y.segment(y_segment(0),y_segment(1)) = obs.simulate_y();
    }
  }

  // Report back the simulated random effects and data to R
  SIMULATE{
    REPORT(time_effects);
    REPORT(proc_w);
    REPORT(resp_w);
    REPORT(obs_y);
  }


  // Rename parameters so they're easily found on the R side
  // ADREPORT lets you get standard errors
  vector<Type> par_mean_pars = mean_pars;
  REPORT(par_mean_pars);
  ADREPORT(par_mean_pars);

  // Cross-check distribution_code order with family.hpp
  // switch statement doesn't work with REPORT and ADREPORT
  if( distribution_code == 0 ) { // Normal
    Type par_sd = response_pars(0);
    REPORT(par_sd);
    ADREPORT(par_sd);
  } else if( distribution_code == 1 ) { // Poisson
    // no extra pars
  } else if( distribution_code == 2 ) { // Neg. Binomial
    Type par_overdispersion = response_pars(0);
    REPORT(par_overdispersion);
    ADREPORT(par_overdispersion);
  } else if( distribution_code == 3 ) { // Bernoulli
    // no extra pars
  } else if( distribution_code == 4 ) { // Gamma
    Type par_sd = response_pars(0);
    REPORT(par_sd);
    ADREPORT(par_sd);
  } else if( distribution_code == 5 ) { // lognormal
    Type par_sd = response_pars(0);
    REPORT(par_sd);
    ADREPORT(par_sd);
  } else if( distribution_code == 6 ) { // Binomial
    // no extra pars
  } else if( distribution_code == 7 ) { // AtLeastOneBinomial
      // no extra pars
  } else if ( distribution_code == 8 ) { // Conway-Maxwell-Poisson
    Type par_dispersion = response_pars(0);
    REPORT(par_dispersion);
    ADREPORT(par_dispersion);
  } else {}

  Type par_space_sd = space_sd;
  REPORT(par_space_sd);
  ADREPORT(par_space_sd);

  Type par_space_rho = space_rho*mean_dist;
  REPORT(par_space_rho);
  ADREPORT(par_space_rho);

  Type par_space_nu = space_nu;
  REPORT(par_space_nu);
  ADREPORT(par_space_nu);

  Type par_time_mu = time_mu;
  REPORT(par_time_mu);
  ADREPORT(par_time_mu);

  Type par_time_ar1 = time_ar1;
  REPORT(par_time_ar1);
  ADREPORT(par_time_ar1);

  Type par_time_sd = time_sd;
  REPORT(par_time_sd);
  ADREPORT(par_time_sd);

  return(nll+pred_nll);
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this
