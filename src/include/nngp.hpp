template<class Type>
class nngp {
  private:
    covariance<Type> cov; // Covariance Function
    Type time_sd; // standard deviation for first random effect (no parents)
    vector<Type> w; // Field value at knots
    vector<Type> mean; // Field mean at knots
    vector<vector<int> > ws_graph; // dag as edge list
    vector<matrix<Type> > ws_dists; // dag edge distances as list of matrices

    kriging<Type> fieldPred(vector<int> parents, matrix<Type> dists,
                            Type marginal_mean, bool interpolate_mean);

  public:
    nngp(covariance<Type> cov,
         Type time_sd,
         vector<Type> w,
         vector<Type> mean,
         vector<vector<int> > ws_graph,
         vector<matrix<Type> > ws_dists);
    nngp() = default;

    void update_w(vector<Type> new_w,
                  vector<Type> new_mean);

    vector<Type> get_w() { return w; }
    Type loglikelihood();
    vector<Type> predict_w(vector<vector<int> > into_edges,
                           vector<matrix<Type> > dists,
                           vector<Type> pred_w,
                           Type &nll);
    vector<Type> simulate();
    vector<Type> simulate_resp_w(vector<vector<int> > resp_w_edges,
                                 vector<matrix<Type> > resp_w_dists);
};



template<class Type>
nngp<Type>::nngp(covariance<Type> cov,
                 Type time_sd,
                 vector<Type> w,
                 vector<Type> mean,
                 vector<vector<int> > ws_graph,
                 vector<matrix<Type> > ws_dists) :
  cov(cov),
  time_sd(time_sd),
  w(w),
  mean(mean),
  ws_graph(ws_graph),
  ws_dists(ws_dists) {
  // Nothing left to initialize
}


template<class Type>
kriging<Type> nngp<Type>::fieldPred(vector<int> parents,
                                    matrix<Type> dists,
                                    Type marginal_mean,
                                    bool interpolate_mean) {
  matrix<Type> covMat = cov(dists);
  for(int i=0; i<parents.size(); i++) {
    if( ws_graph(parents(i)).size() == 0 ) {
      Rcout << covMat << "\n\n";
      for(int j=0; j<covMat.rows(); j++) {
        covMat(i,j) *= time_sd/cov.get_tau();
        covMat(j,i) *= time_sd/cov.get_tau();
        // covMat(i+1,j) = 0;
        // covMat(j,i+1) = 0;
      }
      Rcout << covMat << "\n\n\n\n\n";
    } else {}
  }


  vector<Type> meanVec(covMat.cols());
  meanVec(0) = marginal_mean;
  for(int i=0; i<parents.size(); i++) {
    meanVec(i+1) = mean(parents(i));
  }

  vector<Type> predictor_vals(parents.size());
  for(int i=0; i<parents.size(); i++) {
    predictor_vals(i) = w(parents(i));
  }

  kriging<Type> krig(covMat, meanVec, predictor_vals, interpolate_mean);
  return krig;
}


template<class Type>
void nngp<Type>::update_w(vector<Type> new_w,
                          vector<Type> new_mean) {
  w = new_w; // I shouldn't need to resizeLike(new_w), but maybe.
  mean = new_mean;
}


template<class Type>
Type nngp<Type>::loglikelihood() {
  Type ans = 0.0;
  // Type ans = dnorm(w(0), mean(0), time_sd, true);
  for(int i=0; i<ws_graph.size(); i++) {
    if( ws_graph(i).size() == 0 ) {
      ans += dnorm(w(i), mean(i), time_sd, true);
    } else {
      kriging<Type> krig = fieldPred(ws_graph(i),
                                     ws_dists(i),
                                     mean(i),
                                     false);
      ans += dnorm(w(i), krig.mean(), krig.sd(), true);
    }
  }
  return ans;
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
vector<Type> nngp<Type>::simulate() {
  // w(0) = rnorm(mean(0), time_sd);
  for(int i=0; i<w.size(); i++) {
    if( ws_graph(i).size() == 0 ) {
      w(i) = rnorm(mean(i), time_sd);
    } else {
      kriging<Type> krig = fieldPred(ws_graph(i),
                                     ws_dists(i),
                                     mean(i),
                                     false);
      w(i) = rnorm(krig.mean(), krig.sd());
    }
  }

  return w;
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
