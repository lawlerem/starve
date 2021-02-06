template<class Type>
class covariance {
  private:
    Type scaleTau;
    Type rho;
    Type nu;
    Type marg_var;

    int covar_code;

    template<typename T> T dist(T d);
    template<typename T> T covFun(T d);

  public:
    covariance(Type scaleTau, Type rho, Type nu, int covar_code);
    covariance() = default;

    Type get_scaleTau();
    void update_scaleTau(Type new_tau); // Updates scaleTau, NOT marginal variance
    void update_marg_sd(Type new_marg_sd); // Updates marginal variance, NOT scaleTau

    template<typename T> T operator() (T d);
    template<typename T> vector<T> operator() (vector<T> d);
    template<typename T> matrix<T> operator() (matrix<T> D);
};


// Initializer
template<class Type>
covariance<Type>::covariance(Type scaleTau, Type rho, Type nu, int covar_code) :
  scaleTau(scaleTau),
  rho(rho),
  nu(nu),
  covar_code(covar_code) {
  switch(covar_code) {
    case 1 : this->marg_var = pow(scaleTau,2); break;// Gaussian, nu=Inf
    default : this->marg_var = pow(scaleTau*pow(rho,nu),2); break;
  }
}



template<class Type>
Type covariance<Type>::get_scaleTau() {
  return this->scaleTau;
}
// Update scaleTau
template<class Type>
void covariance<Type>::update_scaleTau(Type new_tau) {
  this->scaleTau = new_tau;
  switch(covar_code) {
    case 1 : this->marg_var = pow(new_tau,2); break; // Gaussian
    default : this->marg_var = pow(new_tau*pow(rho,nu),2); break;
  }
}



// Update marg_var
template<class Type>
void covariance<Type>::update_marg_sd(Type new_marg_sd) {
  switch(covar_code) {
    case 1 : this->marg_var = pow(new_marg_sd,2); break; // Gaussian, don't change rho
    default : this->marg_var = pow(new_marg_sd,2);
              this->rho = pow(marg_var/pow(scaleTau,2),1/(2*nu)); break;
  }
}



// Distance function -- private
template<class Type>
template<typename T>
T covariance<Type>::dist(T d) {
  T ans = sqrt(d*d);
  return ans;
}

// Covariance function -- private
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
