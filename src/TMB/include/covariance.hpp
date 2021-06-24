// Class that computes covariances and covariance matrices
//
// Evaluate x(...) to compute covariances from distances
template<class Type>
class covariance {
  private:
    Type scaleTau; // Identifiable ratio of variance to range (= marg_var / rho^(2*nu))
    Type nu; // Smoothness parameter

    int covar_code; // Which covariance function to use?

    template<typename T> T dist(T d); // Compute distances (can update later for anisotropy)
    template<typename T> T covFun(T d); // Compute covariance given distance

  public:
    // Constructor
    covariance(Type scaleTau, Type rho, Type nu, int covar_code);
    covariance() = default;

    Type marg_var; // Marginal variance
    Type rho; // Spatial range
    Type get_scaleTau() { return this->scaleTau; } // Get scale tau

    // Set value for scaleTau, range held constat, marg_var re-computed
    void update_scaleTau(Type new_tau);
    // Set value for marginal variance, scaleTau held constant, range re-computed
    void update_marg_sd(Type new_marg_sd);

    // Compute covariances
    template<typename T> T operator() (T d);
    template<typename T> vector<T> operator() (vector<T> d);
    template<typename T> matrix<T> operator() (matrix<T> D);
};


// Constructor
template<class Type>
covariance<Type>::covariance(Type scaleTau, Type rho, Type nu, int covar_code) :
  scaleTau(scaleTau),
  rho(rho),
  nu(nu),
  covar_code(covar_code) {
  switch(covar_code) {
    case 1 : this->marg_var = pow(scaleTau,2); break; // Gaussian, nu=Inf so let marg_var=scaleTau
    default : this->marg_var = pow(scaleTau*pow(rho,nu),2); break;
  }
}

template<class Type>
void covariance<Type>::update_scaleTau(Type new_tau) {
  this->scaleTau = new_tau;
  switch(covar_code) {
    case 1 : this->marg_var = pow(new_tau,2); break; // Gaussian
    default : this->marg_var = pow(new_tau*pow(rho,nu),2); break; // Matern
  }
}

/*
var = pow(tau*pow(rho,nu),2)
sd = tau*pow(rho,nu)
sd / tau = rho^nu
rho = nu-rt(sd / tau)
*/

template<class Type>
void covariance<Type>::update_marg_sd(Type new_marg_sd) {
  this->marg_var = pow(new_marg_sd,2);
  switch(covar_code) {
    case 1 : break; // Gaussian, don't change rho
    default : this->rho = pow(sqrt(marg_var)/scaleTau,1/nu); break; // Matern
  }
}



// Distance function
template<class Type>
template<typename T>
T covariance<Type>::dist(T d) {
  T ans = sqrt(d*d);
  return ans;
}

// Covariance function
template<class Type>
template<typename T>
T covariance<Type>::covFun(T d) {
  switch(covar_code) {
    case 0 : return (T) marg_var * exp( -dist(d)/(T)rho ); // Exponential
    case 1 : return (T) marg_var * exp( -pow(dist(d)/(T)rho,2) ); // Gaussian
    case 2 : return (T) marg_var * matern(d, rho, nu); // Matern
    case 3 : return (T) marg_var * (1 + sqrt(3)*dist(d)/(T)rho) * exp( -sqrt(3)*dist(d)/(T)rho ); // Matern32 (nu = 1.5)
    default : return (T) marg_var * matern(d, rho, nu); // Matern
  }
}

// Covariance function operator -- single
template<class Type>
template<typename T>
T covariance<Type>::operator() (T d) {
  return covFun(d);
}

// Covariance function operator -- vector
template<class Type>
template<typename T>
vector<T> covariance<Type>::operator() (vector<T> d) {
  vector<T> ans(d.size());
  for(int i=0; i<d.size(); i++) {
    ans(i) = covFun(d(i));
  }
  return ans;
}

// Covariance function operator -- matrix
template<class Type>
template<typename T>
matrix<T> covariance<Type>::operator() (matrix<T> D) {
  matrix<T> ans(D.rows(), D.cols());
  for(int i=0; i<D.rows(); i++) {
    for(int j=0; j<D.cols(); j++) {
      ans(i,j) = covFun(D(i,j));
    }
  }
  return ans;
}
