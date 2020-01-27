struct inv_link_function {
    int link_code;

    template<class T>
    T operator() (T linear) {
        switch(link_code) {
          case 0 : return linear; // identity
          case 1 : return exp(linear); // log
          case 2 : return invlogit(linear); // logit
          default : return linear; // identity
        }
    }
};

struct response_density {
  int distribution_code;

  template<class T>
  T operator() (T data,T mean ,vector<T> pars) {
    switch(distribution_code) {
      case 0 : return dnorm(data,mean,exp(pars(0)),true); // Normal
      case 1 : return dpois(data,mean,true); // Poisson
      case 2 : return dnbinom2(data,mean,(exp(pars(0))+1)*mean,true); // Neg. Binomial
      case 3 : return dbinom(data,T(1),mean,true); // Bernoulli with p = mean
      default : return dnorm(data,mean,exp(pars(0)),true); // Normal
    }
  }
};