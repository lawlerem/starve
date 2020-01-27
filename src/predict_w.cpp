#include <TMB.hpp>
#include <iostream>
using namespace density;

#include "include/covariance.hpp"
#include "include/kriging.hpp"

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_MATRIX(distances);
  DATA_MATRIX(variance_inflation);
  DATA_VECTOR(this_time_w);
  DATA_VECTOR(last_time_w);
  DATA_INTEGER(return_type);

  PARAMETER(logtau);
  PARAMETER(logrho);
  PARAMETER(logit_w_phi);

  Type tau = exp(logtau);
  Type rho = exp(logrho);
  Type w_phi = invlogit(logit_w_phi);

  vector<Type> parameters(3);
  parameters << logtau, logrho, logit_w_phi;

  covariance<Type> cov(tau,rho);
  vector<Type> full_mean(this_time_w.size()+1);
  full_mean << 0, w_phi*last_time_w;

  matrix<Type> full_covariance = cov(distances) + variance_inflation;
  kriging<Type> krig(full_covariance,
                     full_mean,
                     this_time_w,
                     true);

  if( return_type == 1 ) {
    return krig.var();
  } else {
    return krig.mean();
  }
}
