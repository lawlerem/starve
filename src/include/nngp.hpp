template<class Type>
struct mvnorm {
  matrix<Type> cov;
  vector<Type> w;
  vector<Type> mu;
};

// A class to represent a nearest-neighbour Gaussian process
//
// Used to compute likelihood for a spatial field, predict at new locations, and
//   simulate a new spatial field.
template<class Type>
class nngp {
  private:
    covariance<Type> cov; // Covariance Function
    vector<Type> w; // Spatial random effects
    vector<Type> mean; // Mean of spatial
    vector<vector<int> > ws_graph; // Edge list for persistent graph
    vector<matrix<Type> > ws_dists; // Distances for persistent graph

    // Get covariance matrix, random effects, and mean vector
    mvnorm<Type> joint(vector<int> nodes,
                       matrix<Type> dists);

    // Compute kriging predictor given list of edges, distances, and magrinal
    // mean for new location
    kriging<Type> fieldPred(vector<int> parents, matrix<Type> dists,
                            Type marginal_mean, bool interpolate_mean);

    Type avg_forecast_sd; // Average kriging standard deviation for random effects
  public:
    // Constructor
    nngp(covariance<Type> cov,
         vector<Type> w,
         vector<Type> mean,
         vector<vector<int> > ws_graph,
         vector<matrix<Type> > ws_dists);
    nngp() = default;

    // Write over the random effects and mean
    void update_w(vector<Type> new_w,
                  vector<Type> new_mean);

    vector<Type> get_w() { return w; }

    // Compute loglikelihood for random effects
    Type loglikelihood();

    // Compute kriging predictor for new locations, and add their
    // likelihood contribution to a likelihood function (passed by reference,
    //   so nll is updated automatically).
    vector<Type> predict_w(vector<vector<int> > into_edges,
                           vector<matrix<Type> > dists,
                           vector<Type> pred_w,
                           Type &nll);
    // Simulate random effects
    vector<Type> simulate();

    // Simulate random effects for random effects not part of persistent graph
    vector<Type> simulate_resp_w(vector<vector<int> > resp_w_edges,
                                 vector<matrix<Type> > resp_w_dists);
};



template<class Type>
nngp<Type>::nngp(covariance<Type> cov,
                 vector<Type> w,
                 vector<Type> mean,
                 vector<vector<int> > ws_graph,
                 vector<matrix<Type> > ws_dists) :
  cov(cov),
  w(w),
  mean(mean),
  ws_graph(ws_graph),
  ws_dists(ws_dists) {
  // Calibrate scaleTau coefficient (avg_forecast_sd/cov.get_scaleTau) so
  // that scaleTau gives average forecast variance
  avg_forecast_sd = 0.0;
  for( int i=1; i<ws_graph.size(); i++ ) {
    avg_forecast_sd += fieldPred(ws_graph(i),
                                 ws_dists(i),
                                 Type(0.0),
                                 false).sd();
  }
  avg_forecast_sd *= 1.0/(ws_graph.size()-1);
  // range held constant, marginal variance re-computed
  this->cov.update_scaleTau(cov.get_scaleTau()/(avg_forecast_sd/cov.get_scaleTau()));

  // For starting random effects, we'll need to recompute avg_forecast_sd
  avg_forecast_sd = 0.0;
  for( int i=1; i<ws_graph.size(); i++ ) {
    avg_forecast_sd += fieldPred(ws_graph(i),
                                 ws_dists(i),
                                 Type(0.0),
                                 false).sd();
  }
  avg_forecast_sd *= 1.0/(ws_graph.size()-1);
}


// parents = which random effects are predictors?
// dists = distance from prediction point to predictor locations
// marginal_mean = marginal mean of prediction point
// interpolate_mean =  should the marginal_mean be replaced with a local estimate?
template<class Type>
kriging<Type> nngp<Type>::fieldPred(vector<int> parents,
                                    matrix<Type> dists,
                                    Type marginal_mean,
                                    bool interpolate_mean) {
  // Compute covariance matrix from distance matrix
  matrix<Type> covMat = cov(dists);

  // Combine means of prediction point and predictors
  vector<Type> meanVec(covMat.cols());
  meanVec(0) = marginal_mean;
  for(int i=0; i<parents.size(); i++) {
    meanVec(i+1) = mean(parents(i));
  }

  // Get the predictor values
  vector<Type> predictor_vals(parents.size());
  for(int i=0; i<parents.size(); i++) {
    predictor_vals(i) = w(parents(i));
  }

  // Compute kriging predictor
  kriging<Type> krig(covMat, meanVec, predictor_vals, interpolate_mean);
  return krig;
}


template<class Type>
void nngp<Type>::update_w(vector<Type> new_w,
                          vector<Type> new_mean) {
  w = new_w; // Don't need to resizeLike(new_w) since same number every time
  mean = new_mean;
}


// Get covariance matrix, random effects, and mean
template<class Type>
mvnorm<Type> nngp<Type>::joint(vector<int> nodes,
                               matrix<Type> dists) {
  matrix<Type> covMat = cov(dists);
  vector<Type> meanVec(nodes.size());
  vector<Type> wVec(nodes.size());
  for(int i=0; i<meanVec.size(); i++) {
    meanVec(i) = mean(nodes(i));
    wVec(i) = w(nodes(i));
  }
  mvnorm<Type> ans = {covMat, wVec, meanVec};
  return ans;
}



// Compute log-likelihood for random effects
template<class Type>
Type nngp<Type>::loglikelihood() {
  Type ans = 0.0;
  int offset = 0;
  Type old_sd = sqrt(cov(Type(0.0)));
  for(int i=0; i<ws_graph.size(); i++) {
    if( i == 0 | ws_graph(i)(0) < 0 ) {
      offset += ws_graph(i).size()-1;
      cov.update_marg_sd(avg_forecast_sd);
      mvnorm<Type> mvn = joint(abs(ws_graph(i)), ws_dists(i));
      ans += -1*MVNORM(mvn.cov)(mvn.w-mvn.mu); // MVNORM calculates neg. log lik.
      cov.update_marg_sd(old_sd);
    } else {
      kriging<Type> krig = fieldPred(ws_graph(i),
                                     ws_dists(i),
                                     mean(i+offset),
                                     false);
      ans += dnorm(w(i+offset), krig.mean(), krig.sd(), true);
    }
  }

  return ans;
}

template<class Type>
vector<Type> nngp<Type>::simulate() {
  int offset = 0;
  Type old_sd = sqrt(cov(Type(0.0)));
  for(int i=0; i<ws_graph.size(); i++) {
    if( i == 0 | ws_graph(i)(0) < 0 ) {
      offset += ws_graph(i).size()-1;
      cov.update_marg_sd(avg_forecast_sd);
      mvnorm<Type> mvn = joint(abs(ws_graph(i)), ws_dists(i));
      vector<Type> simW = MVNORM(mvn.cov).simulate()+mvn.mu;
      for(int j=0; j<simW.size(); j++) {
        w(abs(ws_graph(i)(j))) = simW(j);
      }
      cov.update_marg_sd(old_sd);
    } else {
      kriging<Type> krig = fieldPred(ws_graph(i),
                                     ws_dists(i),
                                     mean(i+offset),
                                     false);
      w(i+offset) = rnorm(krig.mean(), krig.sd());
    }
  }

  return w;
}


template<class Type>
vector<Type> nngp<Type>::predict_w(vector<vector<int> > into_edges,
                                   vector<matrix<Type> > dists,
                                   vector<Type> pred_w,
                                   Type &nll) {
  for(int i=0; i<pred_w.size(); i++) {
    kriging<Type> krig = fieldPred(into_edges(i),
                                   dists(i),
                                   Type(0), // This value doesn't matter since
                                   true); // we interpolate it.
    nll -= dnorm(pred_w(i), krig.mean(), krig.sd(), true);
  }
  return pred_w;
}

template<class Type>
vector<Type> nngp<Type>::simulate_resp_w(vector<vector<int> > resp_w_edges,
                                         vector<matrix<Type> > resp_w_dists) {
  vector<Type> sim_w(resp_w_edges.size());
  for(int i=0; i<sim_w.size(); i++) {
    kriging<Type> krig = fieldPred(resp_w_edges(i),
                                   resp_w_dists(i),
                                   Type(0), // This value doesn't matter since
                                   true); // we interpolate it
    sim_w(i) = rnorm(krig.mean(), krig.sd());
  }

  return sim_w;
}
