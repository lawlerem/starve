template<class Type>
class observations {
  private:
    nngp<Type> &process;
    vector<Type> y;
    data_indicator<vector<Type>,Type> keep; // DATA_VECTOR_INDICATOR for OSA residuals
    vector<vector<int> > ys_graph; // Parents for each observation
    vector<matrix<Type> > ys_dists; // Distances for graph
    vector<Type> link_w;
    matrix<Type> mean_design;
    glm<Type> family;

    vector<Type> response;

  public:
    observations(nngp<Type> &process,
                 vector<Type> y,
                 data_indicator<vector<Type>,Type> keep,
                 vector<vector<int> > ys_graph,
                 vector<matrix<Type> > ys_dists,
                 vector<Type> link_w,
                 matrix<Type> mean_design,
                 glm<Type> family);
    observations() = default;

    void update_y(vector<Type> new_y,
               data_indicator<vector<Type>,Type> new_keep,
               vector<vector<int> > new_graph,
               vector<matrix<Type> > new_dists,
               vector<Type> new_link_w,
               matrix<Type> new_mean_design);

    vector<Type> find_response();
    Type link_w_loglikelihood();
    Type y_loglikelihood();
    vector<Type> predict_y(vector<Type> pred_w,
                           matrix<Type> pred_mean_design);
};

template<class Type>
observations<Type>::observations(nngp<Type> &process,
                                 vector<Type> y,
                                 data_indicator<vector<Type>,Type> keep,
                                 vector<vector<int> > ys_graph,
                                 vector<matrix<Type> > ys_dists,
                                 vector<Type> link_w,
                                 matrix<Type> mean_design,
                                 glm<Type> family) :
  process(process),
  y(y),
  keep(keep),
  ys_graph(ys_graph),
  ys_dists(ys_dists),
  link_w(link_w),
  mean_design(mean_design),
  family(family) {
  this->response = find_response();
}


template<class Type>
void observations<Type>::update_y(vector<Type> new_y,
                                  data_indicator<vector<Type>,Type> new_keep,
                                  vector<vector<int> > new_graph,
                                  vector<matrix<Type> > new_dists,
                                  vector<Type> new_link_w,
                                  matrix<Type> new_mean_design) {
  keep = new_keep;
  ys_graph = new_graph; // shouldn't need to resizeLike(new_graph), but maybe
  y = new_y;
  ys_dists = new_dists;
  link_w = new_link_w;
  mean_design = new_mean_design;
  this->response = find_response();
};


template<class Type>
vector<Type> observations<Type>::find_response() {
  vector<Type> ans(y.size());
  int link_w_idx = 0;
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i);
    Type field;
    if( parents.size() == 1 ) {
      field = process.get_w()(parents(0));
    } else {
      field = link_w(link_w_idx);
      link_w_idx++;
    }
    ans(i) = family.inv_link(vector<Type>(mean_design.row(i)), field);
  }
  return ans;
}

template<class Type>
Type observations<Type>::link_w_loglikelihood() {
  Type ans = 0.0;
  int link_w_idx = 0;
  vector<vector<int> > link_w_edges(link_w.size());
  vector<matrix<Type> > link_w_dists(link_w.size());
  for(int i=0; i<ys_graph.size(); i++) {
    vector<int> parents = ys_graph(i);
    if( parents.size() == 1 ) {
      continue;
    } else {
      link_w_edges(link_w_idx) = parents;
      link_w_dists(link_w_idx) = ys_dists(i);
      link_w_idx++;
    }
  }

  process.predict_w(link_w_edges,link_w_dists,link_w,ans);
  return -1*ans;
}

template<class Type>
Type observations<Type>::y_loglikelihood() {
  Type ans = 0.0;
  response = find_response();
  for(int i=0; i<ys_graph.size(); i++) {
    ans += keep(i)*family.log_density(Type(y(i)), response(i));
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
