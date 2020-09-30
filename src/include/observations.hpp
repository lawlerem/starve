template<class Type>
class observations {
  private:
    nngp<Type> &process;
    vector<Type> y;
    data_indicator<vector<Type>,Type> keep; // DATA_VECTOR_INDICATOR for OSA residuals
    vector<vector<int> > ys_graph; // Parents for each observation
    vector<matrix<Type> > ys_dists; // Distances for graph
    vector<Type> resp_w;
    matrix<Type> mean_design;
    vector<int> sample_size;
    glm<Type> family;

    vector<Type> response;

  public:
    observations(nngp<Type> &process,
                 vector<Type> y,
                 data_indicator<vector<Type>,Type> keep,
                 vector<vector<int> > ys_graph,
                 vector<matrix<Type> > ys_dists,
                 vector<Type> resp_w,
                 matrix<Type> mean_design,
                 vector<int> sample_size,
                 glm<Type> family);
    observations() = default;

    void update_y(vector<Type> new_y,
               data_indicator<vector<Type>,Type> new_keep,
               vector<vector<int> > new_graph,
               vector<matrix<Type> > new_dists,
               vector<Type> new_resp_w,
               matrix<Type> new_mean_design,
               vector<int> new_sample_size);

    vector<Type> find_response();
    Type resp_w_loglikelihood();
    Type y_loglikelihood();
    vector<Type> predict_y(vector<Type> pred_w,
                           matrix<Type> pred_mean_design);
    vector<Type> simulate_resp_w();
    vector<Type> simulate_y();
};

template<class Type>
observations<Type>::observations(nngp<Type> &process,
                                 vector<Type> y,
                                 data_indicator<vector<Type>,Type> keep,
                                 vector<vector<int> > ys_graph,
                                 vector<matrix<Type> > ys_dists,
                                 vector<Type> resp_w,
                                 matrix<Type> mean_design,
                                 vector<int> sample_size,
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
                                  vector<vector<int> > new_graph,
                                  vector<matrix<Type> > new_dists,
                                  vector<Type> new_resp_w,
                                  matrix<Type> new_mean_design,
                                  vector<int> new_sample_size) {
  keep = new_keep;
  ys_graph = new_graph; // shouldn't need to resizeLike(new_graph), but maybe
  y = new_y;
  ys_dists = new_dists;
  resp_w = new_resp_w;
  mean_design = new_mean_design;
  sample_size = new_sample_size;
  this->response = find_response();
};


template<class Type>
vector<Type> observations<Type>::find_response() {
  vector<Type> ans(y.size());
  int resp_w_idx = 0;
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i);
    Type field;
    if( parents.size() == 1 ) {
      field = process.get_w()(parents(0));
    } else {
      field = resp_w(resp_w_idx);
      resp_w_idx++;
    }
    ans(i) = family.inv_link(vector<Type>(mean_design.row(i)), field);
  }
  return ans;
}

template<class Type>
Type observations<Type>::resp_w_loglikelihood() {
  Type ans = 0.0;
  int resp_w_idx = 0;
  vector<vector<int> > resp_w_edges(resp_w.size());
  vector<matrix<Type> > resp_w_dists(resp_w.size());
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i);
    if( parents.size() == 1 ) {
      continue;
    } else {
      resp_w_edges(resp_w_idx) = parents;
      resp_w_dists(resp_w_idx) = ys_dists(i);
      resp_w_idx++;
    }
  }

  process.predict_w(resp_w_edges,resp_w_dists,resp_w,ans);
  return -1*ans;
}

template<class Type>
Type observations<Type>::y_loglikelihood() {
  Type ans = 0.0;
  response = find_response();
  for(int i=0; i<ys_graph.size(); i++) {
    ans += keep(i)*family.log_density(Type(y(i)), response(i), sample_size(i));
  }
  return ans;
}

template<class Type>
vector<Type> observations<Type>::predict_y(vector<Type> pred_w,
                                           matrix<Type> pred_mean_design) {
  vector<Type> pred_y(pred_w.size());
  for(int i=0; i<pred_y.size(); i++) {
    pred_y(i) = family.inv_link(vector<Type>(pred_mean_design.row(i)),pred_w(i));
  }
  return pred_y;
}

template<class Type>
vector<Type> observations<Type>::simulate_resp_w() {
  int resp_w_idx = 0;
  vector<vector<int> > resp_w_edges(resp_w.size());
  vector<matrix<Type> > resp_w_dists(resp_w.size());
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i);
    if( parents.size() == 1 ) {
      continue;
    } else {
      resp_w_edges(resp_w_idx) = parents;
      resp_w_dists(resp_w_idx) = ys_dists(i);
      resp_w_idx++;
    }
  }

  resp_w = process.simulate_resp_w(resp_w_edges,resp_w_dists);

  return resp_w;
}

template<class Type>
vector<Type> observations<Type>::simulate_y() {
  response = find_response();
  for(int i=0; i<ys_graph.size(); i++) {
    y(i) = family.simulate(response(i), sample_size(i));
  }

  return y;
}
