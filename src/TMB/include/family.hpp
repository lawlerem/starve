struct inv_link_function {
  int code;

  template<class Type>
  Type operator() (Type x) {
    switch(code) {
      case 0 : return x; // identity
      case 1 : return exp(x); // log
      case 2 : return invlogit(x); // logit
      default : return x; // identity
    }
  }
};

template<class Type>
struct distribution {
  int code;
  vector<Type> pars;

  // Compute log-density
  template<class T>
  T operator() (T data,T mean,T size) {
    switch(code) {
      case 0 : return dnorm(data,mean,pars(0),true); // Normal
      case 1 : return dpois(data,mean,true); // Poisson
      case 2 : return dnbinom2(data,mean,pars(0)*mean,true); // Neg. Binomial
      case 3 : return dbinom(data,T(1),mean,true); // Bernoulli with p = mean
      case 4 : return dgamma(data,pow(mean/pars(0),2),pow(pars(0),2)/mean,true);
                                  // Gamma; shape = (mu/sd)^2/,     scale = sd^2/mu
      case 5 : return dnorm(log(data),mean,pars(0),true) - log(data); // Log-normal
      case 6 : return dbinom(data,T(size),mean,true); // Binomial
      case 7 : return (data == 0 ?
                        size*log(1-mean) :
                        log(1 - pow(1-mean,size)) ); // AtLeastOneBinomial
      case 8 : return dcompois2(data,mean,pars(0),true); // Conway-Maxwell-Poisson
      case 9 : return dtweedie(data,
        // size*pars(0)*mean, // E[y]
        size*mean, // E[y]
        // exp(log(mean)-log(2-pars(1)))*pow(size*pars(0)*mean,1-pars(1)), // dispersion
        pars(0), // dispersion
        pars(1), // power
        true
      ); // Tweedie
      default : return dnorm(data,mean,pars(0),true); // Normal
    }
  }

  // simulate a single draw
  template<class T>
  T simulate(T mean,T size) {
    switch(code) {
      case 0 : return rnorm(mean,pars(0)); // Normal
      case 1 : return rpois(mean); // Poisson
      case 2 : return rnbinom2(mean,pars(0)*mean); // Neg. Binomial
      case 3 : return rbinom(T(1),mean); // Bernoulli with p = mean
      case 4 : return rgamma(pow(mean/pars(0),2),pow(pars(0),2)/mean);
                                  // shape = mu^2/var,     scale = var/mu
      case 5 : return exp(rnorm(mean,pars(0))); // Log-normal
      case 6 : return rbinom(T(size),mean); // Binomial
      case 7 : return (rbinom(T(size),mean) == 0 ? 0 : 1); // AtLeastOneBinomial
      case 8 : return rcompois2(mean,pars(0)); // Conway-Maxwell-Poisson
      // case 9 : return rtweedie(size*pars(0)*mean,exp(log(mean)-log(2-pars(1)))*pow(size*pars(0)*mean,1-pars(1)),pars(1)); // Tweedie
      case 9 : return rtweedie(size*mean,pars(0),pars(1)); // Tweedie
      default : return rnorm(mean,pars(0)); // Normal
    }
  }
};



template<class Type>
class family {
  private:
    inv_link_function ilink;
    distribution<Type> dist;
  public:
    family(
      const inv_link_function& ilink,
      const distribution<Type>& dist
    ) : ilink{ilink}, dist{dist} {};
    family() = default;

    Type operator() (Type x, Type lmean, Type sample_size) {return dist(x,ilink(lmean),sample_size);}
    Type simulate(Type lmean, Type sample_size) {return dist.simulate(ilink(lmean),sample_size);}
};
