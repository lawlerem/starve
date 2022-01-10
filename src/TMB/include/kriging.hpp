// A class to facilitate kriging predictions, or i.e. conditional normal distributions
//
// Constructing this object computes the kriging (Gaussian conditional) mean
// and variance from the joint Gaussian distribution
template<class Type>
class kriging {
  private:
    vector<Type> krig_mean; // Conditional mean of prediction points given predictors
    matrix<Type> krig_cov; // Conditional covariance of prediction points given predictors
    matrix<Type> krig_Q; // Inverse of krig_cov

    matrix<Type> c_SigmaInv; // Sigma_12 * Sigma_22^-1
    bool interpolate_mean;
  public:
    // Constructor
    kriging(matrix<Type> full_covariance,
            vector<Type> full_mean,
            vector<Type> predictor_vals,
            bool interpolate_mean);
    kriging() = default;

    void update_mean(vector<Type> predictor_vals,
                     vector<Type> full_mean);

    // Accessors
    vector<Type> mean() { return this->krig_mean; }
    // Type sd() { return this->ans_sd; }
    matrix<Type> cov() { return this->krig_cov; }
    matrix<Type> Q() { return this->krig_Q; }
};

// Some utility functions for kriging
namespace krig_funs {
  template<class Type> matrix<Type> get_inv(matrix<Type> mat);
  template<class Type> vector<Type> interpolate_mean(vector<Type> predictor_means,
                                                     matrix<Type> c_SigmaInv);
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
  interpolate_mean(interpolate_mean) {
  int n_predictor = predictor_vals.size();
  int n_predictee = full_mean.size()-n_predictor;

  // Get different blocks of the joint covariance matrix
  matrix<Type> pred_covariance = full_covariance.topLeftCorner(n_predictee,n_predictee);
  matrix<Type> cross_covariance = full_covariance.topRightCorner(n_predictee,n_predictor);
  matrix<Type> predictor_covariance = full_covariance.bottomRightCorner(n_predictor,n_predictor);
  matrix<Type> predictor_precision = krig_funs::get_inv(predictor_covariance);
  c_SigmaInv = cross_covariance * predictor_precision;

  // krig. var = Sigma_11 - Sigma_12 * Sigma_22^-1 * Sigma_12^T
  this->krig_cov = pred_covariance - c_SigmaInv * cross_covariance.transpose();
  this->krig_Q = atomic::matinv(this->krig_cov);

  update_mean(predictor_vals,full_mean);
}


template<class Type>
void kriging<Type>::update_mean(vector<Type> predictor_vals,
                                vector<Type> full_mean) {
  int n_predictor = predictor_vals.size();
  int n_predictee = full_mean.size() - n_predictor;
  vector<Type> predictor_means = full_mean.segment(n_predictee,n_predictor);
  vector<Type> pred_means(n_predictee);
  if( interpolate_mean ) {
    pred_means = krig_funs::interpolate_mean(predictor_means,
                                             c_SigmaInv);
  } else {
    pred_means = full_mean.segment(0,n_predictee);
  }
  this->krig_mean = pred_means + vector<Type>(c_SigmaInv * (predictor_vals - predictor_means).matrix());
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
vector<Type> krig_funs::interpolate_mean(vector<Type> predictor_means,
                                         matrix<Type> c_SigmaInv) {
  // numerator = Sigma_12 * Sigma^-1 * mu; kriging predictor applied to mean
  vector<Type> num = vector<Type>(c_SigmaInv * predictor_means.matrix());

  vector<Type> ones(predictor_means.size());
  for(int i=0; i<ones.size(); i++) {
    ones(i) = Type(1);
  }
  // denom = Sigma_12 * Sigma^-1 * 1; ensures weights (coefficients of pred_means) sum to one
  vector<Type> denom = vector<Type>(c_SigmaInv * ones.matrix());
  vector<Type> ans(denom.size());
  for(int i=0; i<ans.size(); i++) {
    if( denom(i) == 0 ) {
      ans(i) = 0; // denom == 0 if there are no neighbours found
    } else {
      ans(i) = num(i)/denom(i);
    }
  }

  return ans;
}
