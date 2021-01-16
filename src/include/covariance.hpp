template<class Type>
class covariance {
  private:
    Type scaleTau;
    Type rho;
    Type nu;
    Type sqtau;

    int covar_code;

    template<typename T> T dist(T d);
    template<typename T> T covFun(T d);

  public:
    covariance(Type scaleTau, Type rho, Type nu, int covar_code);
    covariance() = default;

    void update_tau(Type new_tau); // Updates marginal variance, NOT scaleTau

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
    case 1 : this->sqtau = pow(scaleTau,2); break;// Gaussian, nu=Inf
    default : this->sqtau = pow(scaleTau*pow(rho,nu),2); break;
  }
}


// Update tau
template<class Type>
void covariance<Type>::update_tau(Type new_tau) {
  switch(covar_code) {
    case 1 : this->sqtau = pow(new_tau,2); break; // Gaussian, don't change rho
    default : this->sqtau = pow(new_tau,2);
              this->rho = pow(sqtau/pow(scaleTau,2),1/(2*nu)); break;
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
  // return (T)sqtau * exp( -dist(d) /(T)rho ); // Exponential
  switch(covar_code) {
    case 0 : return (T) sqtau * exp( -dist(d)/(T)rho ); // Exponential
    case 1 : return (T) sqtau * exp( -pow(dist(d)/(T)rho,2) ); // Gaussian
    case 2 : return (T) sqtau * matern(d, rho, nu); // Matern
    case 3 : return (T) sqtau * (1 + sqrt(3)*dist(d)/(T)rho) * exp( -sqrt(3)*dist(d)/(T)rho ); // Matern32 (nu = 1.5)
    default : return (T)sqtau * matern(d, rho, nu); // Matern
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
