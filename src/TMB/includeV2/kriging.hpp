template<class Type>
class conditional_normal {
  private:
    matrix<Type> sigma;
    int nc; // Number of components conditioned on
    int np; // Number of components in resulting conditional distribution
    matrix<Type> c_sigma_inv; // Sigma_12 * Sigma_22^-1
    MVNORM_t<Type> mvn;
  public:
    conditional_normal(
      matrix<Type> sigma,
      int nc // The last nc components will be conditioned on
    );
    conditional_normal() = default;

    // Compute conditional negative log-likelihood
    Type operator() (
      vector<Type> x,
      vector<Type> mu,
      bool interpolate_mu=false
    );

    vector<Type> simulate(
      vector<Type> x,
      vector<Type> mu,
      bool interpolate_mu=false
    );

    vector<Type> conditional_mean(
      vector<Type> x,
      vector<Type> mu,
      bool interpolate_mu=false
    );
};


template<class Type>
conditional_normal<Type>::conditional_normal(
    matrix<Type> sigma,
    int nc
  ) :
  sigma(sigma),
  nc(nc) {
    np = sigma.rows() - nc;

    matrix<Type> sigma11 = sigma.topLeftCorner(np,np);
    matrix<Type> sigma12 = sigma.topRightCorner(np,nc);
    matrix<Type> sigma22 = sigma.bottomRightCorner(nc,nc);
    matrix<Type> inv22 = sigma22;
    if( inv22.rows() > 0 ) {
      inv22 = atomic::matinv(inv22);
    } else {}

    c_sigma_inv = sigma12*inv22;
    matrix<Type> sigma_p = sigma11 - c_sigma_inv*sigma12.transpose();
    mvn = MVNORM_t<Type>(sigma_p);
}


template<class Type>
Type conditional_normal<Type>::operator() (
    vector<Type> x,
    vector<Type> mu,
    bool interpolate_mu
  ) {
  vector<Type> x_p = x.segment(0,np);
  vector<Type> mu_p = conditional_mean(x,mu,interpolate_mu);
  return mvn(x_p-mu_p);
}


template<class Type>
vector<Type> conditional_normal<Type>::simulate(
    vector<Type> x,
    vector<Type> mu,
    bool interpolate_mu
  ) {
  vector<Type> mu_p = conditional_mean(x,mu,interpolate_mu);
  vector<Type> x_p = mvn.simulate() + mu_p;

  return x_p;
}


template<class Type>
vector<Type> conditional_normal<Type>::conditional_mean(
    vector<Type> x,
    vector<Type> mu,
    bool interpolate_mu
  ) {
  vector<Type> x_p = x.segment(0,np);
  vector<Type> mu_p = mu.segment(0,np);
  vector<Type> x_c = x.segment(np,nc);
  vector<Type> mu_c = mu.segment(np,nc);

  if( interpolate_mu ) {
    vector<Type> num = vector<Type>(c_sigma_inv * mu_c.matrix());
    vector<Type> ones(nc); ones.setZero(); ones = 1.0+ones;
    vector<Type> denom = vector<Type>(c_sigma_inv * ones.matrix());
    for(int p=0; p<np; p++) {
      if( denom(p) == 0 ) {
        mu_p(p) = 0.0;
      } else {
        mu_p(p) = num(p)/denom(p);
      }
    }
  } else {}
  vector<Type> conditional_mean = mu_p + vector<Type>(c_sigma_inv * (x_c - mu_c).matrix());

  return conditional_mean;
}
