template<class Type>
class time_series {
  private:
    array<Type> re; // random effects [time,var]
    array<Type> pars; // parameters [par,var], par = [mu,ar1,sd]
  public:
    time_series(
      array<Type> re,
      array<Type> pars
    );
    time_series() = default;

    // Accessor methods
    array<Type> get_re() { return re; }
    time_series<Type> slice_t(int start, int length);
    time_series<Type> slice_v(int start, int length);

    Type loglikelihood();
    time_series<Type> simulate();
    vector<Type> propagate_structure(array<Type> new_re,int t,int v);
    array<Type> propagate_structure(array<Type> new_re);
};


template<class Type>
time_series<Type>::time_series(
    array<Type> re,
    array<Type> pars
  ) :
  re(re),
  pars(pars) {
    // Nothing left to initialize
}


template<class Type>
time_series<Type> time_series<Type>::slice_t(int start,int length) {
  array<Type> new_re(re.dim(1),length);
  for(int t=0; t<length; t++) {
    new_re.col(t) = re.transpose().col(t+start);
  }
  new_re = new_re.transpose();

  time_series<Type> new_ts(
    new_re,
    pars
  );

  return new_ts;
}


template<class Type>
time_series<Type> time_series<Type>::slice_v(int start,int length) {
  array<Type> new_re(re.dim(0),length);
  array<Type> new_pars(pars.dim(0),length);
  for(int v=0; v<length; v++) {
    new_re.col(v) = re.col(v+start);
    new_pars.col(v) = pars.col(v+start);
  }

  time_series<Type> new_ts(
    new_re,
    new_pars
  );

  return new_ts;
}



template<class Type>
Type time_series<Type>::loglikelihood() {
  Type ans = 0.0;
  Type small_number = pow(10.0,-5);
  for(int v=0; v<re.cols(); v++) {
    ans += dnorm(re(0,v),
                 pars(0,v),
                 (re.rows() > 1) ? pars(2,v) : small_number,
                 true);
    for(int t=1; t<re.rows(); t++) {
      ans += dnorm(re(t,v),
                   pars(1,v)*re(t-1,v) + (1.0-pars(1,v))*pars(0,v),
                   pars(2,v)*sqrt(1.0-pow(pars(1,v),2)),
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
                    (re.rows() > 1) ? pars(2,v) : small_number);
    for(int t=1; t<re.rows(); t++) {
      re(t,v) = rnorm(pars(1,v)*re(t-1,v) + (1.0-pars(1,v))*pars(0,v),
                      pars(2,v)*sqrt(1.0-pow(pars(1,v),2)));
    }
  }

  return *this;
}


template<class Type>
vector<Type> time_series<Type>::propagate_structure(
  array<Type> new_re,
  int t,
  int v
  ) {
    vector<Type> pred(new_re.dim(0));
    for(int s=0; s<new_re.dim(0); s++) {
      if( t==0 ) {
        pred(s) = re(0,v);
      } else {
        pred(s) = pars(1,v)*(new_re(s,t-1) - re(t-1,v)) + re(t,v);
      }
    }

    return pred;
}


template<class Type>
array<Type> time_series<Type>::propagate_structure(
    array<Type> new_re
  ) {
  array<Type> pred(new_re.dim(0),new_re.dim(1),new_re.dim(2));
  for(int v=0; v<new_re.dim(2); v++) {
    for(int t=0; t<new_re.dim(1); t++) {
      pred.col(v).col(t) = propagate_structure(new_re.col(v),t,v);
    }
  }
  return pred;
}
