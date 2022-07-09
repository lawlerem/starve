// Class that computes covariances and covariance matrices
//
// Evaluate x(...) to compute covariances from distances
template<class Type>
class covariance {
  private:
    vector<Type> pars;
    int covar_code; // Which covariance function to use?

  public:
    // Constructor
    covariance(const vector<Type>& pars, const int& covar_code) : pars{pars}, covar_code{covar_code} {};
    covariance() : pars{vector<Type>()}, covar_code(0) {};

    // Compute covariances
    template<typename T> T operator() (const T& d);
    template<typename T> vector<T> operator() (const vector<T>& d);
    template<typename T> matrix<T> operator() (const matrix<T>& d);

    void update_marginal_sd(const Type& new_sd);
};



template<class Type>
template<typename T>
T covariance<Type>::operator() (const T& d) {
  switch(covar_code) {
    case 0 : return (T) pow(pars(0),2) * pars(1) * exp( -d/(T)pars(1) ); // Exponential [sd, range]
    case 1 : return (T) pow(pars(0),2) * exp( -pow(d/(T)pars(1),2) ); // Gaussian [marg_sd, range]
    case 2 : return (T) pow(pars(0),2) * pow(pars(1),2*pars(2)) * matern(d,pars(1),pars(2)); // Matern [sd, range, nu]
    case 3 : return (T) pow(pars(0),2) * (1+sqrt(3.0)*d/(T)pars(1)) * exp( -sqrt(3.0)*d/(T)pars(1) ); // Matern32 [sd, range]
    default : return (T) pow(pars(0),2) * pars(1) * exp( -d/(T)pars(1) ); // Exponential [sd, range]
  }
}

template<class Type>
template<typename T>
vector<T> covariance<Type>::operator() (const vector<T>& d) {
  vector<T> ans(d.size());
  for(int i=0; i<d.size(); i++) {
    ans(i) = operator()(d(i));
  }
  return ans;
}

template<class Type>
template<typename T>
matrix<T> covariance<Type>::operator() (const matrix<T>& d) {
  matrix<T> ans(d.rows(),d.cols());
  for(int i=0; i<d.rows(); i++) {
    for(int j=0; j<d.cols(); j++) {
      ans(i,j) = operator()(d(i,j));
    }
  }
  return ans;
}

template<class Type>
void covariance<Type>::update_marginal_sd(const Type& new_sd) {
  switch(covar_code) {
    case 0 : pars(1) = pow(new_sd/pars(0),1.0/(0.5)); // Exponential
    case 1 : pars(1) = new_sd; // Gaussian
    case 2 : pars(1) = pow(new_sd/pars(0),1.0/pars(2)); // Matern
    case 3 : pars(1) = pow(new_sd/pars(0),1.0/(3.0/2.0)); // Matern32
    default : pars(1) = pow(new_sd/pars(0),1.0/(0.5)); // Exponential
  }
}
