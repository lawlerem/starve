template<class Type>
class nngp {
  private:
    covariance<Type> cov; // Covariance Function
    vector<Type> w; // Field value at knots
    vector<Type> mean; // Field mean at knots
    vector<vector<int> > ws_graph; // dag as edge list
    vector<matrix<Type> > ws_dists; // dag edge distances as list of matrices

    kriging<Type> fieldPred(vector<int> parents, matrix<Type> dists,
                            Type marginal_mean, bool interpolate_mean);

  public:
    nngp(covariance<Type> cov,
         vector<Type> w,
         vector<Type> mean,
         vector<vector<int> > ws_graph,
         vector<matrix<Type> > ws_dists);
    nngp() = default;

    Type avg_forecast_sd;

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
                 vector<Type> w,
                 vector<Type> mean,
                 vector<vector<int> > ws_graph,
                 vector<matrix<Type> > ws_dists) :
  cov(cov),
  w(w),
  mean(mean),
  ws_graph(ws_graph),
  ws_dists(ws_dists) {
  // Calibrate scaleTau coefficient so that scaleTau gives average
  // forecast variance
  avg_forecast_sd = 0.0;
  for( int i=1; i<ws_graph.size(); i++ ) {
    avg_forecast_sd += fieldPred(ws_graph(i),
                                 ws_dists(i),
                                 Type(0.0),
                                 false).sd();
  }
  avg_forecast_sd *= 1.0/(ws_graph.size()-1);
  this->cov.update_scaleTau(cov.get_scaleTau()/(avg_forecast_sd/cov.get_scaleTau()));

  avg_forecast_sd = 0.0;
  for( int i=1; i<ws_graph.size(); i++ ) {
    avg_forecast_sd += fieldPred(ws_graph(i),
                                 ws_dists(i),
                                 Type(0.0),
                                 false).sd();
  }
  avg_forecast_sd *= 1.0/(ws_graph.size()-1);

  // Calibrate initSD so that initial random effect has comparable
  // variance to rest of random effects
  Type init_sd = fieldPred(ws_graph(0).segment(1,ws_graph(0).size()-1),
                          ws_dists(0),
                          Type(0.0),
                          false).sd();
  this->cov.update_marg_sd(init_sd);
}


template<class Type>
kriging<Type> nngp<Type>::fieldPred(vector<int> parents,
                                    matrix<Type> dists,
                                    Type marginal_mean,
                                    bool interpolate_mean) {
  matrix<Type> covMat = cov(dists);

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
  matrix<Type> covMat = cov(ws_dists(0));
  vector<Type> meanVec(ws_graph(0).size());
  vector<Type> wVec(ws_graph(0).size());
  for(int i=0; i<meanVec.size(); i++) {
    meanVec(i) = mean(ws_graph(0)(i));
    wVec(i) = w(ws_graph(0)(i));
  }
  Type ans = -MVNORM(covMat)(wVec-meanVec); // Times -1 because we want the positive log-likelihood

  int offset = ws_graph(0).size()-1; // ws_graph[[1]] is for the k'th random effect (k = ws_graph(0).size())
  for(int i=1; i<ws_graph.size(); i++) {
    kriging<Type> krig = fieldPred(ws_graph(i),
                                   ws_dists(i),
                                   mean(i+offset),
                                   false);
    ans += dnorm(w(i+offset), krig.mean(), krig.sd(), true);
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
  matrix<Type> covMat = cov(ws_dists(0));
  vector<Type> meanVec(ws_graph(0).size());
  for(int i=0; i<meanVec.size(); i++) {
    meanVec(i) = mean(ws_graph(0)(i));
  }
  vector<Type> simW = MVNORM(covMat).simulate()+meanVec;
  for(int i=0; i<simW.size(); i++) {
    w(ws_graph(0)(i)) = simW(i);
  }

  int offset = ws_graph(0).size()-1; // ws_graph[[1]] is for the k'th random effect (k = ws_graph(0).size())
  for(int i=1; i<ws_graph.size(); i++) {
    kriging<Type> krig = fieldPred(ws_graph(i),
                                   ws_dists(i),
                                   mean(i+offset),
                                   false);
    w(i+offset) = rnorm(krig.mean(), krig.sd());
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
