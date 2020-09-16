template<class Type>
class glm {
  private:
    inv_link_function inv_link_fun;
    response_density distribution;

    Type mu;
    vector<Type> fixed_effects;
    vector<Type> distribution_pars;

  public:
    glm(inv_link_function inv_link,
        response_density distribution,
        Type mu,
        vector<Type> fixed_effects,
        vector<Type> distribution_pars);
    glm() = default;

    Type inv_link(vector<Type> fixed_predictors,
                  Type random_predictor);
    Type log_density(Type x, Type mean, int sample_size);
};


template<class Type>
glm<Type>::glm(inv_link_function inv_link_fun,
               response_density distribution,
               Type mu,
               vector<Type> fixed_effects,
               vector<Type> distribution_pars) :
  inv_link_fun(inv_link_fun),
  distribution(distribution),
  mu(mu),
  fixed_effects(fixed_effects),
  distribution_pars(distribution_pars) {
    // Nothing left to initialize
}

template<class Type>
Type glm<Type>::inv_link(vector<Type> fixed_predictors,
                         Type random_predictor) {
  Type linear_pred = mu + (fixed_predictors*fixed_effects).sum() + random_predictor;
  Type ans = inv_link_fun(linear_pred);
  return ans;
}

template<class Type>
Type glm<Type>::log_density(Type x, Type mean, int sample_size) {
  return distribution(x,mean,sample_size,distribution_pars);
}
