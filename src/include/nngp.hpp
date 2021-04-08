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
    vector<vector<vector<int> > > ws_graph; // Edge list for persistent graph
    vector<matrix<Type> > ws_dists; // Distances for persistent graph

    vector<MVNORM_t<Type> > ws_joints; // Store MVNORM_t object for nodes w/o parents
    vector<kriging<Type> > ws_krigs; // Store kriging objects for nodes w/ parents

    Type avg_forecast_sd; // Average kriging standard deviation for random effects

    // Get covariance matrix, random effects, and mean vector
    mvnorm<Type> joint(vector<int> nodes,
                       matrix<Type> dists);

    // Compute kriging predictor given list of to/from edges, distances, and magrinal
    // mean for new location
    kriging<Type> fieldPred(vector<vector<int> > graph,
                            matrix<Type> dists,
                            bool interpolate_mean);
  public:
    // Constructor
    nngp(covariance<Type> cov,
         vector<Type> w,
         vector<Type> mean,
         vector<vector<vector<int> > > ws_graph,
         vector<matrix<Type> > ws_dists);
    nngp() = default;

    // Write over the random effects and mean
    void update_w(vector<Type> new_w,
                  vector<Type> new_mean);

    vector<Type> get_w() { return w; }

    // Compute loglikelihood for random effects
    Type loglikelihood();

    // Simulate random effects
    vector<Type> simulate();

    // Compute kriging predictor for new locations, and add their
    // likelihood contribution to a likelihood function (passed by reference,
    //   so nll is updated automatically).
    vector<Type> predict_w(vector<vector<vector<int> > > pred_graph,
                           vector<matrix<Type> > dists,
                           vector<Type> pred_w,
                           Type &nll);

    // Simulate random effects for random effects not part of persistent graph
    vector<Type> simulate_resp_w(vector<vector<vector<int> > > resp_w_graph,
                                 vector<matrix<Type> > resp_w_dists);
};



template<class Type>
nngp<Type>::nngp(covariance<Type> cov,
                 vector<Type> w,
                 vector<Type> mean,
                 vector<vector<vector<int> > > ws_graph,
                 vector<matrix<Type> > ws_dists) :
  cov(cov),
  w(w),
  mean(mean),
  ws_graph(ws_graph),
  ws_dists(ws_dists) {
  // Calibrate scaleTau coefficient (avg_forecast_sd/cov.get_scaleTau) so
  // that scaleTau gives average forecast variance
  avg_forecast_sd = 0.0;
  Type n = 0;
  for( int i=0; i<ws_graph.size(); i++ ) {
    if( ws_graph(i)(1).size() > 0 ) {
      kriging<Type> krig = fieldPred(ws_graph(i),
                                      ws_dists(i),
                                      false);
      for(int j=0; j<ws_graph(i)(0).size(); j++) {
        avg_forecast_sd += sqrt(krig.cov()(j,j));
        n += 1.0;
      }
    } else {}
  }
  avg_forecast_sd *= 1.0/n;
  // // range held constant, marginal variance re-computed
  this->cov.update_scaleTau(cov.get_scaleTau()/(avg_forecast_sd/cov.get_scaleTau()));

  ws_krigs.resizeLike(ws_graph);
  avg_forecast_sd = 0.0;
  for( int i=0; i<ws_graph.size(); i++ ) {
    if( ws_graph(i)(1).size() > 1 ) {
      ws_krigs(i) = fieldPred(ws_graph(i),
                              ws_dists(i),
                              false);
      for(int j=0; j<ws_krigs(i).cov().rows(); j++) {
        avg_forecast_sd += sqrt(ws_krigs(i).cov()(j,j));
      }
    } else {}
  }
  avg_forecast_sd *= 1.0/n;

  ws_joints.resizeLike(ws_graph);
  Type old_sd = sqrt(cov(Type(0.0)));
  cov.update_marg_sd(avg_forecast_sd);
  for(int i=0; i<ws_graph.size(); i++) {
    if( ws_graph(i)(1).size() == 0 ) { // If there are no parents
      MVNORM_t<Type> mvn(cov(ws_dists(i)));
      ws_joints(i) = mvn;
    } else {}
  }
  cov.update_marg_sd(old_sd);
}


// graph = list with to and from vertices
// dists = distance from prediction point to predictor locations
// marginal_mean = marginal mean of prediction point
// interpolate_mean =  should the marginal_mean be replaced with a local estimate?
template<class Type>
kriging<Type> nngp<Type>::fieldPred(vector<vector<int> > graph,
                                    matrix<Type> dists,
                                    bool interpolate_mean) {
  vector<int> all_nodes(graph(0).size()+graph(1).size());
  for(int i=0; i<all_nodes.size(); i++) {
    if( i<graph(0).size() ) {
      all_nodes(i)=graph(0)(i); // "To" nodes
    } else {
      all_nodes(i)=graph(1)(i-graph(0).size()); // "From" nodes
    }
  }
  mvnorm<Type> mvn = joint(all_nodes, dists);

  vector<Type> predictor_vals(graph(1).size());
  for(int i=0; i<graph(1).size(); i++) {
    predictor_vals(i) = w(graph(1)(i));
  }

  // Compute kriging predictor
  kriging<Type> krig(mvn.cov, mvn.mu, predictor_vals, interpolate_mean);
  return krig;
}


template<class Type>
void nngp<Type>::update_w(vector<Type> new_w,
                          vector<Type> new_mean) {
  w = new_w; // Don't need to resizeLike(new_w) since same number every time
  mean = new_mean;
  for( int i=0; i<ws_graph.size(); i++ ) {
    if( ws_graph(i)(1).size() > 1 ) {
      vector<int> all_nodes(ws_graph(i)(0).size()+ws_graph(i)(1).size());
      for(int j=0; j<all_nodes.size(); j++) {
        if( j<ws_graph(i)(0).size() ) {
          all_nodes(j)=ws_graph(i)(0)(j); // "To" nodes
        } else {
          all_nodes(j)=ws_graph(i)(1)(j-ws_graph(i)(0).size()); // "From" nodes
        }
      }
      ws_krigs(i).update_mean(new_w(ws_graph(i)(1)),
                              new_mean(all_nodes));
    } else {}
  }
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
  for(int i=0; i<ws_graph.size(); i++) {
    if( ws_graph(i)(1).size() == 0 ) { // If no parents
      mvnorm<Type> mvn = joint(ws_graph(i)(0), ws_dists(i));
      ans += -1*ws_joints(i)(mvn.w-mvn.mu);
    } else {
      vector<Type> this_w(ws_graph(i)(0).size());
      for(int j=0; j<ws_graph(i)(0).size(); j++) {
        this_w(j) = w(ws_graph(i)(0)(j));
      }
      ans += -1*MVNORM(ws_krigs(i).cov())(this_w-ws_krigs(i).mean());
    }
  }

  return ans;
}

template<class Type>
vector<Type> nngp<Type>::simulate() {
  Type ans = 0.0;
  for(int i=0; i<ws_graph.size(); i++) {
    if( ws_graph(i)(1).size() == 0 ) { // If no parents
      mvnorm<Type> mvn = joint(ws_graph(i)(0), ws_dists(i));
      vector<Type> simW = ws_joints(i).simulate()+mvn.mu;
      for(int j=0; j<simW.size(); j++) {
        w(ws_graph(i)(0)(j)) = simW(j);
      }
    } else {
      vector<Type> simW = MVNORM(ws_krigs(i).cov()).simulate()+ws_krigs(i).mean();
      for(int j=0; j<simW.size(); j++) {
        w(ws_graph(i)(0)(j)) = simW(j);
      }
    }
  }

  return w;
}


template<class Type>
vector<Type> nngp<Type>::predict_w(vector<vector<vector<int> > > pred_graph,
                                   vector<matrix<Type> > dists,
                                   vector<Type> pred_w,
                                   Type &nll) {
  for(int i=0; i<pred_graph.size(); i++) {
    vector<Type> this_w = pred_w(pred_graph(i)(0));
    pred_graph(i)(0).setZero(); // Avoid index errors in fieldPred, actual indices don't matter
    kriging<Type> krig = fieldPred(pred_graph(i),
                                   dists(i),
                                   true); // Interpolate the mean
    nll += MVNORM(krig.cov())(this_w-krig.mean());
  }
  return pred_w;
}

template<class Type>
vector<Type> nngp<Type>::simulate_resp_w(vector<vector<vector<int> > > resp_w_graph,
                                         vector<matrix<Type> > resp_w_dists) {
  vector<Type> sim_w(resp_w_graph.size());
  for(int i=0; i<resp_w_graph.size(); i++) {
    vector<int> this_w = resp_w_graph(i)(0);
    resp_w_graph(i)(0).setZero(); // Avoid index errors in fieldPred, actual indices don't matter
    kriging<Type> krig = fieldPred(resp_w_graph(i),
                                   resp_w_dists(i),
                                   true); // Interpolate the mean
    vector<Type> small_sim_w = MVNORM(krig.cov()).simulate()+krig.mean();
    for(int j=0; j<small_sim_w.size(); j++) {
      sim_w(this_w(j)) = small_sim_w(j);
    }
  }

  return sim_w;
}
