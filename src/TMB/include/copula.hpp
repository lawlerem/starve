template<class Type>
class mvn_copula {
  private:
    matrix<Type> R;
    Type logdetR;
    matrix<Type> Q;
    matrix<Type> I;
    MVNORM_t<Type> MVN;
  public:
    mvn_copula(matrix<Type> R);
    mvn_copula() = default;

    Type operator() (vector<Type> w_std) {
      Type nll = Type(0.5)*logdetR + Type(0.5)*(w_std*vector<Type>(matrix<Type>(Q-I)*w_std)).sum();
      return nll;
    }

    vector<Type> simulate() {
      vector<Type> sim_w_std(R.cols());
      sim_w_std = MVN.simulate();
      return sim_w_std;
    };
};



template<class Type>
mvn_copula<Type>::mvn_copula(matrix<Type> R) :
  R(R) {
  logdetR = atomic::logdet(R);
  Q = atomic::matinv(R);
  I.resizeLike(R); I.setIdentity();
  MVNORM_t<Type> tmp(R);
  MVN = tmp;
}
