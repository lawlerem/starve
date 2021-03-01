// A class to facilitate kriging predictions, or i.e. conditional normal distributions
//
// Constructing this object computes the kriging (Gaussian conditional) mean
// and variance from the joint Gaussian distribution
template<class Type>
class kriging {
  private:
    Type marginal_covariance; // Marginal variance of prediction point
    vector<Type> cross_covariance; // Covariance between prediction point and predictors
    matrix<Type> predictor_covariance; // Covariance matrix for predictors
    matrix<Type> predictor_precision; // Inverse of predictor_covariancce

    Type marginal_mean; // Marginal mean of prediction point
    vector<Type> predictor_means; // Marginal mean of predictors

    vector<Type> predictor_vals; // Realized value of predictors

    Type ans_mean; // Conditional mean of prediction point given predictors
    Type ans_sd; // Conditiona std.dev. of prediction point given predictors
  public:
    // Constructor
    kriging(matrix<Type> full_covariance,
            vector<Type> full_mean,
            vector<Type> predictor_vals,
            bool interpolate_mean);
    kriging() = default;

    // Accessors
    Type mean() { return this->ans_mean; }
    Type sd() { return this->ans_sd; }
    Type var() { return pow(this->ans_sd,2); }
};

// Some utility functions for kriging
namespace krig_funs {
  template<class Type> matrix<Type> get_inv(matrix<Type> mat);
  template<class Type> Type interpolate_mean(vector<Type> predictor_means,
                                             vector<Type> cross_covariance,
                                             matrix<Type> predictor_precision);
}



// Constructor -- this does most of the work
// full_covariance -- Joint covariance of prediction point and predictors
// full_mean -- marginal means of prediction point and predictors
// predictor_vals -- realized value for predictors
// interpolate_mean -- should the mean of the prediction point be overwritten
//   by an interpolated value of the predictor means?
template<class Type>
kriging<Type>::kriging(matrix<Type> full_covariance,
                       vector<Type> full_mean,
                       vector<Type> predictor_vals,
                       bool interpolate_mean) :
  predictor_vals(predictor_vals) {

  // Get different blocks of the joint covariance matrix
  int nrow = full_covariance.rows();
  int ncol = full_covariance.cols();
  this->marginal_covariance = full_covariance(0,0);
  this->cross_covariance = full_covariance.row(0).segment(1,ncol-1);
  this->predictor_covariance = full_covariance.bottomRightCorner(nrow-1,ncol-1);
  this->predictor_precision = krig_funs::get_inv(this->predictor_covariance);

  // Get different blocks of the marginal mean vector, interpolating the prediction mean if desired
  this->predictor_means = full_mean.segment(1,ncol-1);
  if( interpolate_mean ) {
    this->marginal_mean = krig_funs::interpolate_mean(predictor_means,
                                                      cross_covariance,
                                                      predictor_precision);
  } else {
    this->marginal_mean = full_mean(0);
  }

  // Compute kriging (gaussian conditional) mean and standard deviation
  // krig. mean = mu + c^T * Sigma^-1 * ( w-mu )
  this->ans_mean = marginal_mean +
    (cross_covariance.matrix().transpose() * predictor_precision * (predictor_vals - predictor_means).matrix())(0,0);
  // krig. var = sd^2 - c^T * Sigma^-1 * c
  this->ans_sd = sqrt(marginal_covariance -
    (cross_covariance.matrix().transpose() * predictor_precision * cross_covariance.matrix() )(0,0));
}




// Compute matrix inverse in a way that works well with TMB / CppAD
template<class Type>
matrix<Type> krig_funs::get_inv(matrix<Type> mat)  {
  matrix<Type> ans(0,0);
  if( mat.rows() > 0 ) {
    ans.resizeLike(mat);
    ans = atomic::matinv(mat);
  } else {}
  return ans;
}

// Compute a weighted average of predictor means based on covariance matrix
// Gives a (local) BLU estimate of the mean in ordinary kriging
template<class Type>
Type krig_funs::interpolate_mean(vector<Type> predictor_means,
                                 vector<Type> cross_covariance,
                                 matrix<Type> predictor_precision) {
  Type ans;
  // numerator = c^T * Sigma^-1 * mu; kriging predictor applied to mean
  Type num = (cross_covariance.matrix().transpose() * predictor_precision *
                predictor_means.matrix())(0,0); // Need (0,0) because it returns a 1x1 matrix and we want the entry

  vector<Type> ones(predictor_means.size());
  for(int i=0; i<ones.size(); i++) {
    ones(i) = Type(1);
  }
  // denom = c^T * Sigma^-1 * 1; ensures weights (coefficients of pred_means) sum to one
  Type denom = (cross_covariance.matrix().transpose() * predictor_precision *
                  ones.matrix() )(0,0);
  if( denom == 0 ) {
    ans = 0; // denom == 0 if there are no neighbours found
  } else {
    ans = num/denom;
  }

  return ans;
}
