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
    vector<Type> w_std; // Standardized version of random effects
    vector<Type> mean; // Mean of spatial
    vector<vector<vector<int> > > ws_graph; // Edge list for persistent graph
    vector<matrix<Type> > ws_dists; // Distances for persistent graph

    vector<MVNORM_t<Type> > ws_joints; // Store MVNORM_t object for nodes w/o parents
    vector<kriging<Type> > ws_krigs; // Store kriging objects for nodes w/ parents

    Type avg_forecast_sd; // Average kriging standard deviation for random effects
    vector<kriging<Type> > pred_krigs; // Store kriging objects for predictions

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

    vector<Type> get_standard_w();

    // Compute loglikelihood for random effects
    Type loglikelihood();

    // Simulate random effects
    vector<Type> simulate(vector<Type> standard_w);

    // Compute kriging predictor for new locations, and add their
    // likelihood contribution to a likelihood function (passed by reference,
    //   so nll is updated automatically).
    vector<Type> predict_w(vector<vector<vector<int> > > pred_graph,
                           vector<matrix<Type> > dists,
                           vector<Type> pred_w,
                           vector<Type> &std_pred_w,
                           Type &nll,
                           bool use_cache=false,
                           bool overwrite_cache=false);

    // Simulate random effects for random effects not part of persistent graph
    vector<Type> simulate_resp_w(vector<Type> sim_standard_resp_w,
                                 vector<vector<vector<int> > > resp_w_graph,
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
  w_std.resizeLike(w);
  // Fill in kriging predictors for nodes with parents
  ws_krigs.resizeLike(ws_graph);
  avg_forecast_sd = 0.0;
  Type n = 0;
  for( int i=0; i<ws_graph.size(); i++ ) {
    if( ws_graph(i)(1).size() > 0 ) {
      ws_krigs(i) = fieldPred(ws_graph(i),
                                      ws_dists(i),
                                      false);
      for(int j=0; j<ws_graph(i)(0).size(); j++) {
        avg_forecast_sd += sqrt(ws_krigs(i).cov()(j,j));
        n += 1.0;
      }
    } else {}
  }
  avg_forecast_sd *= 1.0/n;

  // Fill in joint distributions for nodes without parents
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


// Outputs kriging predictor for a location(s) given the dag component and distances
//   to random effect nodes.
//
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


// Updates random effects to new values, updates marginal means, updates
//   conditional means in ws_krigs. Don't need to update ws_joints because it only
//   holds the covariance matrix.
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


// Get covariance matrix, random effects, and mean given nodes and distance matrix.
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



// Compute standardized random effects
template<class Type>
vector<Type> nngp<Type>::get_standard_w() {
  for(int i=0; i<ws_graph.size(); i++) {
    vector<Type> tmp_std;
    if( ws_graph(i)(1).size() == 0 ) { // If no parents
      mvnorm<Type> mvn = joint(ws_graph(i)(0), ws_dists(i));
      tmp_std.resizeLike(mvn.w);
      tmp_std = spd_sqrt(ws_joints(i).Q)*vector<Type>(mvn.w-mvn.mu);
    } else {
      vector<Type> this_w(ws_graph(i)(0).size());
      for(int j=0; j<ws_graph(i)(0).size(); j++) {
        this_w(j) = w(ws_graph(i)(0)(j));
      }
      tmp_std.resizeLike(this_w);
      tmp_std = spd_sqrt(ws_krigs(i).Q())*vector<Type>(this_w-ws_krigs(i).mean());
    }
    for(int j=0; j<ws_graph(i)(0).size(); j++) {
      w_std(ws_graph(i)(0)(j)) = tmp_std(j);
    }
  }
  return w_std;
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
vector<Type> nngp<Type>::simulate(vector<Type> standard_w) {
  w_std = standard_w;
  for(int i=0; i<ws_graph.size(); i++) {
    vector<Type> this_standard_w(ws_graph(i)(0).size());
    vector<Type> sim_w;
    for(int j=0; j<this_standard_w.size(); j++) {
      this_standard_w(j) = standard_w(ws_graph(i)(0)(j));
    }
    sim_w.resizeLike(this_standard_w);
    if( ws_graph(i)(1).size() == 0 ) { // If no parents
      mvnorm<Type> mvn = joint(ws_graph(i)(0), ws_dists(i));
      sim_w = spd_sqrt(ws_joints(i).Sigma)*this_standard_w + mvn.mu;
    } else {
      vector<int> all_nodes(ws_graph(i)(0).size()+ws_graph(i)(1).size());
      for(int j=0; j<all_nodes.size(); j++) {
        if( j<ws_graph(i)(0).size() ) {
          all_nodes(j)=ws_graph(i)(0)(j); // "To" nodes
        } else {
          all_nodes(j)=ws_graph(i)(1)(j-ws_graph(i)(0).size()); // "From" nodes
        }
      }
      ws_krigs(i).update_mean(w(ws_graph(i)(1)),
                              mean(all_nodes));
      sim_w = spd_sqrt(ws_krigs(i).cov())*this_standard_w + ws_krigs(i).mean();
    }
    for(int j=0; j<sim_w.size(); j++) {
      w(ws_graph(i)(0)(j)) = sim_w(j);
    }
  }

  // vector<Type> foo = get_standard_w();
  // for(int i=0; i<foo.size(); i++) {
  //   Rcout << "w_std in: " << standard_w(i) << "   w_std out: " << foo(i) << "\n";
  // }
  // Rcout << "\n\n";

  return w;
}


template<class Type>
vector<Type> nngp<Type>::predict_w(vector<vector<vector<int> > > pred_graph,
                                   vector<matrix<Type> > dists,
                                   vector<Type> pred_w,
                                   vector<Type> &std_pred_w,
                                   Type &nll,
                                   bool use_cache,
                                   bool overwrite_cache) {
  for(int i=0; i<pred_graph.size(); i++) {
    vector<int> this_w_idx = pred_graph(i)(0);
    vector<Type> this_w = pred_w(this_w_idx);
    vector<Type> this_std_w = this_w;
    pred_graph(i)(0).setZero(); // Avoid index errors in fieldPred, actual indices don't matter
    kriging<Type> krig;
    if( use_cache ) {
      vector<int> all_nodes(pred_graph(i)(0).size()+pred_graph(i)(1).size());
      for(int j=0; j<all_nodes.size(); j++) {
        if( j<pred_graph(i)(0).size() ) {
          all_nodes(j)=pred_graph(i)(0)(j); // "To" nodes
        } else {
          all_nodes(j)=pred_graph(i)(1)(j-pred_graph(i)(0).size()); // "From" nodes
        }
      }
      krig = pred_krigs(i);
      krig.update_mean(w(pred_graph(i)(1)),
                       mean(all_nodes));
    } else {
      krig = fieldPred(pred_graph(i),
                       dists(i),
                       true); // Interpolate the mean
    }
    if( overwrite_cache ) {
      pred_krigs.resizeLike(pred_graph);
      pred_krigs(i) = krig;
    } else {}
    nll += MVNORM(krig.cov())(this_w-krig.mean());
    this_std_w = spd_sqrt(krig.Q())*vector<Type>(this_w-krig.mean());

    for(int j=0; j<this_std_w.size(); j++) {
      std_pred_w(this_w_idx(j)) = this_std_w(j);
    }
  }
  return pred_w;
}

template<class Type>
vector<Type> nngp<Type>::simulate_resp_w(vector<Type> sim_standard_resp_w,
                                         vector<vector<vector<int> > > resp_w_graph,
                                         vector<matrix<Type> > resp_w_dists) {
  vector<Type> sim_w(resp_w_graph.size());
  for(int i=0; i<resp_w_graph.size(); i++) {
    vector<int> this_w = resp_w_graph(i)(0);
    vector<Type> this_sim_standard_resp_w = sim_standard_resp_w(this_w);
    resp_w_graph(i)(0).setZero(); // Avoid index errors in fieldPred, actual indices don't matter
    kriging<Type> krig = fieldPred(resp_w_graph(i),
                                   resp_w_dists(i),
                                   true); // Interpolate the mean
    vector<Type> this_sim_w = spd_sqrt(krig.cov())*this_sim_standard_resp_w + krig.mean();
    for(int j=0; j<this_sim_w.size(); j++) {
      sim_w(this_w(j)) = this_sim_w(j);
    }
  }

  return sim_w;
}
