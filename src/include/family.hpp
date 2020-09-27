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
  T operator() (T data,T mean,int size,vector<T> pars) {
    switch(distribution_code) {
      case 0 : return dnorm(data,mean,exp(pars(0)),true); // Normal
      case 1 : return dpois(data,mean,true); // Poisson
      case 2 : return dnbinom2(data,mean,(exp(pars(0))+1)*mean,true); // Neg. Binomial
      case 3 : return dbinom(data,T(1),mean,true); // Bernoulli with p = mean
      case 4 : return dgamma(data,pow(mean,2)/exp(pars(0)),exp(pars(0))/mean,true);
                                  // shape = mu^2/var,     scale = var/mu
      case 5 : return dnorm(log(data),mean,exp(pars(0)),true) - log(data); // Log-normal
      case 6 : return dbinom(data,T(size),mean,true); // Binomial
      case 7 : return (data == 0 ?
                        size*log(1-mean) :
                        log(1 - pow(1-mean,size)) ); // AtLeastOneBinomial
      default : return dnorm(data,mean,exp(pars(0)),true); // Normal
    }
  }
};
