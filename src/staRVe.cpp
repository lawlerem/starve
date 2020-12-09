#include <TMB.hpp>
#include <iostream>
using namespace density;

#include "include/data_in.hpp"
#include "include/time_segment.hpp"
#include "include/family.hpp"
#include "include/glm.hpp"

#include "include/covariance.hpp"
#include "include/kriging.hpp"
#include "include/nngp.hpp"
#include "include/observations.hpp"




template<class Type>
Type objective_function<Type>::operator() () {
  DATA_INTEGER(n_time);
  DATA_INTEGER(distribution_code);
  DATA_INTEGER(link_code);

  DATA_IVECTOR(y_time);
  DATA_VECTOR(obs_y);
  DATA_VECTOR_INDICATOR(keep,obs_y);
  DATA_STRUCT(ys_edges,directed_graph);
  DATA_STRUCT(ys_dists,dag_dists);

  DATA_IVECTOR(resp_w_time);

  DATA_MATRIX(mean_design);
  DATA_IVECTOR(sample_size);

  DATA_INTEGER(covar_code);
  DATA_IVECTOR(w_time);
  DATA_STRUCT(ws_edges,directed_graph);
  DATA_STRUCT(ws_dists,dag_dists);

  DATA_IVECTOR(pred_w_time);
  DATA_STRUCT(pred_ws_edges,directed_graph);
  DATA_STRUCT(pred_ws_dists,dag_dists);

  DATA_INTEGER(conditional_sim); // If true, don't simulate w

  PARAMETER(mu);
  PARAMETER_VECTOR(working_response_pars); // Response distribution parameters except for mean
  PARAMETER_VECTOR(mean_pars); // Fixed effects B*X
  PARAMETER_VECTOR(resp_w);
  PARAMETER(logScaleTau);
  PARAMETER(logrho);
  PARAMETER(lognu);
  PARAMETER(logit_w_phi);
  PARAMETER(log_time_sd);
  PARAMETER_VECTOR(proc_w);
  PARAMETER_VECTOR(pred_w);

  vector<Type> response_pars = working_response_pars;
  switch(distribution_code) {
    case 0 : response_pars(0) = exp(working_response_pars(0)); break; // Normal
    case 1 : break; // Poisson
    case 2 : response_pars(0) = exp(working_response_pars(0))+1; break; // Neg. Binom.
    case 3 : break; // Bernoulli
    case 4 : response_pars(0) = exp(working_response_pars(0)); break; // Gamma
    case 5 : response_pars(0) = exp(working_response_pars(0)); break; // Log-Normal
    case 6 : break; // Binomial
    case 7 : break; // AtLeastOneBinomai
    default : response_pars(0) = exp(working_response_pars(0)); break; // Normal
  }
  Type rho = exp(logrho);
  Type nu = exp(lognu);
  Type tau;
  switch(covar_code) {
    case 1 : tau = exp(logScaleTau); break;// Gaussian, nu=Inf
    default : tau = exp(logScaleTau)*pow(rho,nu); break;
  }
  Type w_phi = invlogit(logit_w_phi);
  Type time_sd = exp(log_time_sd);

  vector<vector<int> > ys_dag = ys_edges.dag;
  vector<matrix<Type> > ys_dist = ys_dists.dag_dist;

  vector<vector<int> > ws_dag = ws_edges.dag;
  vector<matrix<Type> > ws_dist = ws_dists.dag_dist;

  vector<vector<int> > pred_ws_dag = pred_ws_edges.dag;
  vector<matrix<Type> > pred_ws_dist = pred_ws_dists.dag_dist;

  covariance<Type> cov(tau,rho,nu,covar_code);
  inv_link_function inv_link = {link_code};
  response_density y_density = {distribution_code};
  glm<Type> family(inv_link,
                   y_density,
                   mean_pars,
                   response_pars);

  vector<int> w_segment(2);
  vector<int> y_segment(2);
  vector<int> resp_w_segment(2);
  vector<int> pred_w_segment(2);

  vector<Type> resp_response(obs_y.size());

  Type nll = 0.0;
  Type pred_nll = 0.0;

  // Initial time
  w_segment = get_time_segment(w_time,0);
  y_segment = get_time_segment(y_time,0);
  resp_w_segment = get_time_segment(resp_w_time,0);
  pred_w_segment = get_time_segment(pred_w_time,0);

  nngp<Type> process(cov,
                     time_sd,
                     proc_w.segment(w_segment(0),w_segment(1)),
                     mu+0*proc_w.segment(w_segment(0),w_segment(1)), // vector of mu
                     ws_dag,
                     ws_dist);

  observations<Type> obs(process,
                         obs_y.segment(y_segment(0),y_segment(1)),
                         keep.segment(y_segment(0),y_segment(1)),
                         ys_dag.segment(y_segment(0),y_segment(1)),
                         ys_dist.segment(y_segment(0),y_segment(1)),
                         resp_w.segment(resp_w_segment(0),resp_w_segment(1)),
                         matrix_row_segment(mean_design,y_segment(0),y_segment(1)),
                         sample_size.segment(y_segment(0),y_segment(1)),
                         family);
  resp_response.segment(y_segment(0),y_segment(1)) = obs.find_response();

  process.predict_w(pred_ws_dag,
                    pred_ws_dist,
                    pred_w.segment(pred_w_segment(0),pred_w_segment(1)),
                    pred_nll);

  nll -= process.loglikelihood()
            + obs.resp_w_loglikelihood()
            + obs.y_loglikelihood();
  SIMULATE{
    if( !conditional_sim) {
      proc_w.segment(w_segment(0),w_segment(1)) = process.simulate();
      resp_w.segment(resp_w_segment(0),resp_w_segment(1)) = obs.simulate_resp_w();
    } else {}
    obs_y.segment(y_segment(0),y_segment(1)) = obs.simulate_y();
  }

  for(int time=1; time<n_time; time++) {
    w_segment = get_time_segment(w_time,time);
    y_segment = get_time_segment(y_time,time);
    resp_w_segment = get_time_segment(resp_w_time,time);
    pred_w_segment = get_time_segment(pred_w_time,time);

    process.update_w(proc_w.segment(w_segment(0),w_segment(1)),
                     (1-w_phi)*mu + w_phi*process.get_w());
    obs.update_y(obs_y.segment(y_segment(0),y_segment(1)),
                 keep.segment(y_segment(0),y_segment(1)),
                 ys_dag.segment(y_segment(0),y_segment(1)),
                 ys_dist.segment(y_segment(0),y_segment(1)),
                 resp_w.segment(resp_w_segment(0),resp_w_segment(1)),
                 matrix_row_segment(mean_design,y_segment(0),y_segment(1)),
                 sample_size.segment(y_segment(0),y_segment(1)));
    resp_response.segment(y_segment(0),y_segment(1)) = obs.find_response();

    process.predict_w(pred_ws_dag,
                      pred_ws_dist,
                      pred_w.segment(pred_w_segment(0),pred_w_segment(1)),
                      pred_nll);

    nll -= process.loglikelihood()
              + obs.resp_w_loglikelihood()
              + obs.y_loglikelihood();
    SIMULATE{
      if( !conditional_sim ) {
        proc_w.segment(w_segment(0),w_segment(1)) = process.simulate();
        resp_w.segment(resp_w_segment(0),resp_w_segment(1)) = obs.simulate_resp_w();
      } else {}
      obs_y.segment(y_segment(0),y_segment(1)) = obs.simulate_y();
    }
  }

  SIMULATE{
    REPORT(proc_w);
    REPORT(resp_w);
    REPORT(obs_y);
  }

  REPORT(pred_nll);

  Type par_mu = mu;
  REPORT(par_mu);
  ADREPORT(par_mu);

  vector<Type> par_mean_pars = mean_pars;
  REPORT(par_mean_pars);
  ADREPORT(par_mean_pars);

  // switch statement doesn't work with REPORT and ADREPORT for some reason
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
  } else {}

  Type par_scaleTau = exp(logScaleTau);
  REPORT(par_scaleTau);
  ADREPORT(par_scaleTau);
  Type working_par_logScaleTau = logScaleTau;
  REPORT(working_par_logScaleTau);
  ADREPORT(working_par_logScaleTau);

  Type par_rho = rho;
  REPORT(par_rho);
  ADREPORT(par_rho);
  Type working_par_logrho = logrho;
  REPORT(working_par_logrho);
  ADREPORT(working_par_logrho);

  Type par_nu = nu;
  REPORT(par_nu);
  ADREPORT(par_nu);
  Type working_par_nu = nu;
  REPORT(working_par_nu);
  ADREPORT(working_par_nu);

  Type par_w_phi = w_phi;
  REPORT(par_w_phi);
  ADREPORT(par_w_phi);
  Type working_par_logit_w_phi = logit_w_phi;
  REPORT(working_par_logit_w_phi);
  ADREPORT(working_par_logit_w_phi);

  Type par_time_sd = time_sd;
  REPORT(par_time_sd);
  ADREPORT(par_time_sd);
  Type working_par_log_time_sd = log_time_sd;
  REPORT(working_par_log_time_sd);
  ADREPORT(working_par_log_time_sd);

  REPORT(obs_y);

  // vector<Type> resp_response = resp_response;
  REPORT(resp_response);
  // ADREPORT(resp_response);

  return(nll+pred_nll);
}
