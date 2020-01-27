template<class Type>
class covariance {
  private:
    Type tau;
    Type rho;
    Type sqtau;
    template<typename T> T dist(T d);
    template<typename T> T covFun(T d);

  public:
    covariance(Type tau, Type rho);
    covariance() = default;

    template<typename T> T operator() (T d);
    template<typename T> vector<T> operator() (vector<T> d);
    template<typename T> matrix<T> operator() (matrix<T> D);
};


// Initializer
template<class Type>
covariance<Type>::covariance(Type tau, Type rho) :
  tau(tau),
  rho(rho) {
  this->sqtau = pow(tau,2);
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
  T cov = (T)sqtau * exp( -dist(d) /(T)rho );
  return cov;
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
