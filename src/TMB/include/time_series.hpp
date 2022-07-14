template<class Type>
class time_series {
  private:
    array<Type> re; // random effects [time,var]
    array<Type> pars; // parameters [par,var], par = [mu,ar1,sd]
  public:
    time_series(
      const array<Type>& re,
      const array<Type>& pars
    ) : re{re}, pars{pars} {};

    // Accessor methods
    array<Type> get_re() { return re; } // return random effect array
    time_series<Type> slice_t(int start, int length); // Get a segment of time t:t+k
    time_series<Type> slice_v(int start, int length); // Get a segment of variable v:v+k
    Type initial_sd_scale(int v) {return 1.0/sqrt(1.0-pow(pars(1,v),2)); }

    // Compute log-likelihood
    Type loglikelihood();

    // Simulate
    time_series<Type> simulate();

    // Get the predicted value of new_re at time t, using the time_series structure of this
    vector<Type> propagate_structure(array<Type>& new_re,int t,int v);
    array<Type> propagate_structure(array<Type>& new_re);
};




template<class Type>
time_series<Type> time_series<Type>::slice_t(int start,int length) {
  array<Type> new_re(length,re.dim(1));
  for(int t=0; t<length; t++) {
    for(int v=0; v<re.dim(1); v++) {
      new_re(t,v) = re(t+start,v);
    }
  }

  return time_series<Type> {new_re, pars};
}


template<class Type>
time_series<Type> time_series<Type>::slice_v(int start,int length) {
  array<Type> new_re(re.dim(0),length);
  array<Type> new_pars(pars.dim(0),length);
  for(int t=0; t<re.dim(0); t++) {
    for(int v=0; v<length; v++) {
      new_re(t,v) = re(t,v+start);
    }
  }
  for(int t=0; t<pars.dim(0); t++) {
    for(int v=0; v<length; v++) {
      new_pars(t,v) = pars(t,v+start);
    }
  }

  return time_series<Type> {new_re, new_pars};
}



template<class Type>
Type time_series<Type>::loglikelihood() {
  Type ans = 0.0;
  Type small_number = pow(10.0,-5);
  for(int v=0; v<re.cols(); v++) {
    ans += dnorm(re(0,v),
                 pars(0,v),
                 (re.rows() > 1) ? initial_sd_scale(v)*pars(2,v) : small_number,
                 true);
    for(int t=1; t<re.rows(); t++) {
      ans += dnorm(re(t,v),
                   pars(1,v)*re(t-1,v) + (1.0-pars(1,v))*pars(0,v),
                   pars(2,v),
                   true);
    }
  }

  return ans;
}


template<class Type>
time_series<Type> time_series<Type>::simulate() {
  Type small_number = pow(10.0,-5);
  for(int v=0; v<re.cols(); v++) {
    re(0,v) = rnorm(pars(0,v),
                    (re.rows() > 1) ? initial_sd_scale(v)*pars(2,v) : small_number);
    for(int t=1; t<re.rows(); t++) {
      re(t,v) = rnorm(pars(1,v)*re(t-1,v) + (1.0-pars(1,v))*pars(0,v),
                      pars(2,v));
    }
  }

  return *this;
}


template<class Type>
vector<Type> time_series<Type>::propagate_structure(
  array<Type>& new_re,
  int t,
  int v
  ) {
    vector<Type> pred(new_re.dim(0));
    for(int s=0; s<new_re.dim(0); s++) {
      if( t==0 ) {
        pred(s) = re(0,v);
      } else {
        pred(s) = pars(1,v)*(new_re(s,t-1,v) - re(t-1,v)) + re(t,v);
      }
    }

    return pred;
}


template<class Type>
array<Type> time_series<Type>::propagate_structure(
    array<Type>& new_re
  ) {
  array<Type> pred(new_re.dim(0),new_re.dim(1),new_re.dim(2));
  for(int s=0; s<new_re.dim(0); s++) {
    for(int v=0; v<new_re.dim(2); v++) {
      for(int t=0; t<new_re.dim(1); t++) {
        if( t==0 ) {
          pred(s,t,v) = re(0,v);
        } else {
          pred(s,t,v) = pars(1,v)*(new_re(s,t-1,v) - re(t-1,v)) + re(t,v);
        }
      }
    }
  }
  return pred;
}
