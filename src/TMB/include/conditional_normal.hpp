template<class Type>
class conditional_normal {
  private:
    matrix<Type> full_sigma; // Full covariance matrix
    int nc; // Number of components conditioned on
    int np; // Number of components in resulting conditional distribution
    matrix<Type> c_sigma_inv; // Sigma_12 * Sigma_22^-1
    MVNORM_t<Type> mvn;
  public:
    conditional_normal(
      const matrix<Type>& full_sigma,
      int nc // The last nc components will be conditioned on
    );
    conditional_normal() = default;

    // Interpolate marginal mean of conditional variables as weighted average of conditioning variables means
    vector<Type> interpolate_mean(const vector<Type>& mu);

    // Compute conditional mean
    vector<Type> conditional_mean(
      const vector<Type>& x,
      const vector<Type>& mu
    );

    // Compute conditional negative log-likelihood
    Type loglikelihood(
      const vector<Type>& x,
      const vector<Type>& mu
    );

    // simulate new
    vector<Type> simulate(
      const vector<Type>& x,
      const vector<Type>& mu
    );

    // Compute condiitonal covariance matrix
    matrix<Type> conditional_cov() { return mvn.cov(); }
};


template<class Type>
conditional_normal<Type>::conditional_normal(
    const matrix<Type>& full_sigma,
    int nc) :
    full_sigma{full_sigma},
    nc{nc},
    np{static_cast<int>(full_sigma.rows()) - nc} {
  matrix<Type> sigma11 = full_sigma.topLeftCorner(np, np);
  matrix<Type> sigma12 = full_sigma.topRightCorner(np, nc);
  matrix<Type> sigma22 = full_sigma.bottomRightCorner(nc, nc);
  matrix<Type> inv22 = sigma22;
  if( inv22.rows() > 0 ) {
    inv22 = atomic::matinv(inv22);
  } else {}

  c_sigma_inv = sigma12 * inv22;
  matrix<Type> sigma_p = sigma11 - c_sigma_inv * sigma12.transpose();
  mvn = MVNORM_t<Type>(sigma_p);
}


template<class Type>
vector<Type> conditional_normal<Type>::interpolate_mean(const vector<Type>& mu) {
  vector<Type> mu_p(np);
  vector<Type> mu_c = mu.segment(np, nc);

  vector<Type> num = vector<Type>(c_sigma_inv * mu_c.matrix());
  vector<Type> ones(nc);
  ones.setZero();
  ones = 1.0 + ones;

  vector<Type> denom = vector<Type>(c_sigma_inv * ones.matrix());
  for(int p = 0; p < np; p++) {
    if( denom(p) == 0 ) {
      mu_p(p) = 0.0;
    } else {
      mu_p(p) = num(p) / denom(p);
    }
  }

  vector<Type> new_mu(mu.size());
  new_mu << mu_p, mu_c;

  return new_mu;
}



template<class Type>
vector<Type> conditional_normal<Type>::conditional_mean(
    const vector<Type>& x,
    const vector<Type>& mu) {
  vector<Type> x_p = x.segment(0, np);
  vector<Type> mu_p = mu.segment(0, np);
  vector<Type> x_c = x.segment(np, nc);
  vector<Type> mu_c = mu.segment(np, nc);

  vector<Type> conditional_mean = mu_p + vector<Type>(
    c_sigma_inv * (x_c - mu_c).matrix()
  );

  return conditional_mean;
}




template<class Type>
Type conditional_normal<Type>::loglikelihood(
    const vector<Type>& x,
    const vector<Type>& mu) {
  vector<Type> x_p = x.segment(0, np);
  vector<Type> mu_p = conditional_mean(x, mu);
  return -1.0 * mvn(x_p - mu_p);
}


template<class Type>
vector<Type> conditional_normal<Type>::simulate(
    const vector<Type>& x,
    const vector<Type>& mu) {
  vector<Type> mu_p = conditional_mean(x, mu);
  vector<Type> x_p = mvn.simulate() + mu_p;

  return x_p;
}
