// A class to hold a generalized linear model and provide related functionality
template<class Type>
class glm {
  private:
    inv_link_function inv_link_fun;
    response_density distribution;

    vector<Type> fixed_effects;
    vector<Type> distribution_pars;

  public:
    glm(inv_link_function inv_link,
        response_density distribution,
        vector<Type> fixed_effects,
        vector<Type> distribution_pars);
    glm() = default;

    // Add together random effects and covariate effects, and convert
    // to response scale for conditional mean
    Type inv_link(vector<Type> fixed_predictors,
                  Type random_predictor);
    // Evaluate the density for an observation x with conditional mean x
    // sample_size is the sample size related to a single binomial observation
    Type log_density(Type x, Type mean, int sample_size);
    // Simulated a single draw from the response distribution
    Type simulate(Type mean, int sample_size);
};


template<class Type>
glm<Type>::glm(inv_link_function inv_link_fun,
               response_density distribution,
               vector<Type> fixed_effects,
               vector<Type> distribution_pars) :
  inv_link_fun(inv_link_fun),
  distribution(distribution),
  fixed_effects(fixed_effects),
  distribution_pars(distribution_pars) {
    // Nothing left to initialize
}

template<class Type>
Type glm<Type>::inv_link(vector<Type> fixed_predictors,
                         Type random_predictor) {
  Type linear_pred = (fixed_predictors*fixed_effects).sum() + random_predictor;
  Type ans = inv_link_fun(linear_pred);
  return ans;
}

template<class Type>
Type glm<Type>::log_density(Type x, Type mean, int sample_size) {
  return distribution(x,mean,sample_size,distribution_pars);
}

template<class Type>
Type glm<Type>::simulate(Type mean, int sample_size) {
  return distribution.simulate(mean,sample_size,distribution_pars);
}
