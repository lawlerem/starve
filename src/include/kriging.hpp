template<class Type>
class kriging {
  private:
    Type marginal_covariance;
    vector<Type> cross_covariance;
    matrix<Type> predictor_covariance;
    matrix<Type> predictor_precision;

    Type marginal_mean;
    vector<Type> predictor_means;

    vector<Type> predictor_vals;

    Type ans_mean;
    Type ans_sd;
  public:
    kriging(matrix<Type> full_covariance,
            vector<Type> full_mean,
            vector<Type> predictor_vals,
            bool interpolate_mean);
    kriging() = default;

    Type mean() { return this->ans_mean; }
    Type sd() { return this->ans_sd; }
    Type var() { return pow(this->ans_sd,2); }
};

namespace krig_funs {
  template<class Type> matrix<Type> get_inv(matrix<Type> mat);
  template<class Type> Type interpolate_mean(vector<Type> predictor_means,
                                             vector<Type> cross_covariance,
                                             matrix<Type> predictor_precision);
}



// Initializer -- this does most of the work
template<class Type>
kriging<Type>::kriging(matrix<Type> full_covariance,
                       vector<Type> full_mean,
                       vector<Type> predictor_vals,
                       bool interpolate_mean) :
  predictor_vals(predictor_vals) {
  int nrow = full_covariance.rows();
  int ncol = full_covariance.cols();
  this->marginal_covariance = full_covariance(0,0);
  this->cross_covariance = full_covariance.row(0).segment(1,ncol-1);
  this->predictor_covariance = full_covariance.bottomRightCorner(nrow-1,ncol-1);
  this->predictor_precision = krig_funs::get_inv(this->predictor_covariance);

  this->predictor_means = full_mean.segment(1,ncol-1);
  if( interpolate_mean ) {
    this->marginal_mean = krig_funs::interpolate_mean(predictor_means,
                                                     cross_covariance,
                                                     predictor_precision);
  } else {
    this->marginal_mean = full_mean(0);
  }

  this->ans_mean = marginal_mean +
    (cross_covariance.matrix().transpose() * predictor_precision * (predictor_vals - predictor_means).matrix())(0,0);
  this->ans_sd = sqrt(marginal_covariance -
    (cross_covariance.matrix().transpose() * predictor_precision * cross_covariance.matrix() )(0,0));
}





template<class Type>
matrix<Type> krig_funs::get_inv(matrix<Type> mat)  {
  matrix<Type> ans(0,0);
  if( mat.rows() > 0 ) {
    ans.resizeLike(mat);
    ans = atomic::matinv(mat);
  } else {}
  return ans;
}

template<class Type>
Type krig_funs::interpolate_mean(vector<Type> predictor_means,
                            vector<Type> cross_covariance,
                            matrix<Type> predictor_precision) {
  Type ans;
  Type num = (cross_covariance.matrix().transpose() * predictor_precision *
                predictor_means.matrix())(0,0); // Need (0,0) because it returns a 1x1 matrix and we want the entry
  vector<Type> ones(predictor_means.size());
  for(int i=0; i<ones.size(); i++) {
    ones(i) = Type(1);
  }
  Type denom = (cross_covariance.matrix().transpose() * predictor_precision *
                  ones.matrix() )(0,0);
  if( denom == 0 ) {
    ans = 0; // denom == 0 if there are no neighbours found
  } else {
    ans = num/denom;
  }

  return ans;
}
