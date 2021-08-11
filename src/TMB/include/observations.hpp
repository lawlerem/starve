// A class to represent the response distribution conditional on random effects
//
// Used to compute likelihood for observations, predict a response mean, and
//   simulate a new response data.
template<class Type>
class observations {
  private:
    nngp<Type> &process; // Pass by reference so that outside updates to process
    // automatically update here as well
    vector<Type> y; // Response data
    data_indicator<vector<Type>,Type> keep; // Used for TMB residuals
    vector<vector<vector<int> > > ys_graph; // Edge list for transient graph
    // The "to" list should have length 1
    vector<matrix<Type> > ys_dists; // Distances for transient graph
    vector<Type> resp_w; // Extra random effects needed for transient graph
    matrix<Type> mean_design; // Covariate observations
    vector<Type> sample_size; // Sample size for binomial observations
    glm<Type> family; // link function and response distribution

    vector<Type> response; // Response mean computed from process and covariates
  public:
    // Constructor
    observations(nngp<Type> &process,
                 vector<Type> y,
                 data_indicator<vector<Type>,Type> keep,
                 vector<vector<vector<int> > > ys_graph,
                 vector<matrix<Type> > ys_dists,
                 vector<Type> resp_w,
                 matrix<Type> mean_design,
                 vector<Type> sample_size,
                 glm<Type> family);
    observations() = default;

    // Overwrite to get new observations, graph, covariates, etc.
    void update_y(vector<Type> new_y,
               data_indicator<vector<Type>,Type> new_keep,
               vector<vector<vector<int> > > new_graph,
               vector<matrix<Type> > new_dists,
               vector<Type> new_resp_w,
               matrix<Type> new_mean_design,
               vector<Type> new_sample_size);

    // Compute response mean conditional on random effects and covaraites
    vector<Type> find_response();
    // Likelihood for extra transient graph random effects
    Type resp_w_loglikelihood();
    // Likelihood for observed data
    Type y_loglikelihood();
    // Get predicted means given random effects and covariates
    vector<Type> predict_y(vector<Type> pred_w,
                           matrix<Type> pred_mean_design);
    vector<Type> simulate_resp_w();
    vector<Type> simulate_y();
};

// Constructor
template<class Type>
observations<Type>::observations(nngp<Type> &process,
                                 vector<Type> y,
                                 data_indicator<vector<Type>,Type> keep,
                                 vector<vector<vector<int> > > ys_graph,
                                 vector<matrix<Type> > ys_dists,
                                 vector<Type> resp_w,
                                 matrix<Type> mean_design,
                                 vector<Type> sample_size,
                                 glm<Type> family) :
  process(process),
  y(y),
  keep(keep),
  ys_graph(ys_graph),
  ys_dists(ys_dists),
  resp_w(resp_w),
  mean_design(mean_design),
  sample_size(sample_size),
  family(family) {
  this->response = find_response();
}

template<class Type>
void observations<Type>::update_y(vector<Type> new_y,
                                  data_indicator<vector<Type>,Type> new_keep,
                                  vector<vector<vector<int> > > new_graph,
                                  vector<matrix<Type> > new_dists,
                                  vector<Type> new_resp_w,
                                  matrix<Type> new_mean_design,
                                  vector<Type> new_sample_size) {
  keep = new_keep;
  ys_graph = new_graph; // don't need to resizeLike(new_graph)
  y = new_y;
  ys_dists = new_dists;
  resp_w = new_resp_w;
  mean_design = new_mean_design;
  sample_size = new_sample_size;
  this->response = find_response();
};


// Find predicted response mean for observations
template<class Type>
vector<Type> observations<Type>::find_response() {
  vector<Type> ans(y.size());
  int resp_w_idx = 0;
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i)(1);
    Type field;
    if( parents.size() == 1 ) {
      // If location is identical to a process.random_effect location, use the random effect
      field = process.get_w()(parents(0));
    } else {
      // If new location, use one of the extra transient graph random effects
      field = resp_w(resp_w_idx);
      resp_w_idx++;
    }
    // Convert to response scale
    ans(ys_graph(i)(0)(0)) = family.inv_link(vector<Type>(mean_design.row(ys_graph(i)(0)(0))), field);
  }
  return ans;
}

// Use process to compute likelihood for extra transient graph random effects
template<class Type>
Type observations<Type>::resp_w_loglikelihood() {
  Type ans = 0.0; // Likelihood component
  int resp_w_idx = 0;
  vector<vector<vector<int> > > resp_w_edges(resp_w.size());
  vector<matrix<Type> > resp_w_dists(resp_w.size());
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i)(1);
    if( parents.size() == 1 ) {
      // If location is identical to a process.random_effect location,
      // don't need to use an extra one
      continue;
    } else {
      // Use an extra random effect
      resp_w_edges(resp_w_idx).resize(2);
      resp_w_edges(resp_w_idx)(0).resize(1);
      resp_w_edges(resp_w_idx)(0)(0) = resp_w_idx;
      resp_w_edges(resp_w_idx)(1) = parents;
      resp_w_dists(resp_w_idx) = ys_dists(i);
      resp_w_idx++;
    }
  }

  // Compute the likelihood for extra random effects
  process.predict_w(resp_w_edges,resp_w_dists,resp_w,ans,false,false);
  return -1*ans;
}

// Use process to simulate new extra transient graph random effects
template<class Type>
vector<Type> observations<Type>::simulate_resp_w() {
  int resp_w_idx = 0;
  vector<vector<vector<int> > > resp_w_edges(resp_w.size());
  vector<matrix<Type> > resp_w_dists(resp_w.size());
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i)(1);
    if( parents.size() == 1 ) {
      // If location is identical to a process.random_effect location,
      // don't need to get an extra one
      continue;
    } else {
      // Get an extra random effect
      resp_w_edges(resp_w_idx).resize(2);
      resp_w_edges(resp_w_idx)(0).resize(1);
      resp_w_edges(resp_w_idx)(0)(0) = resp_w_idx;
      resp_w_edges(resp_w_idx)(1) = parents;
      resp_w_dists(resp_w_idx) = ys_dists(i);
      resp_w_idx++;
    }
  }

  // Simulate the extra random effects
  resp_w = process.simulate_resp_w(resp_w_edges,resp_w_dists);

  return resp_w;
}

// Compute response likelihood
template<class Type>
Type observations<Type>::y_loglikelihood() {
  Type ans = 0.0;
  int idx;
  response = find_response(); // Make sure the response means are up-to-date
  for(int i=0; i<ys_graph.size(); i++) {
    idx = ys_graph(i)(0)(0);
    ans += keep(idx)*family.log_density(Type(y(idx)), response(idx), sample_size(idx));
  }
  return ans;
}

// Simulate new response data
template<class Type>
vector<Type> observations<Type>::simulate_y() {
  response = find_response();
  int idx;
  for(int i=0; i<ys_graph.size(); i++) {
    idx = ys_graph(i)(0)(0);
    y(idx) = family.simulate(response(idx), sample_size(idx));
  }

  return y;
}

// Predict response means for new observations
template<class Type>
vector<Type> observations<Type>::predict_y(vector<Type> pred_w,
                                           matrix<Type> pred_mean_design) {
  vector<Type> pred_y(pred_w.size());
  int idx;
  for(int i=0; i<pred_y.size(); i++) {
    idx = ys_graph(i)(0)(0);
    pred_y(idx) = family.inv_link(vector<Type>(pred_mean_design.row(idx)),pred_w(idx));
  }
  return pred_y;
}
