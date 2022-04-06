template<class Type>
struct persistent_graph_node {
  array<Type> re;
  array<Type> mean;
  dag_node<Type> node;
};

// A class to hold the random effects and graph structure for the persistent random effects
template<class Type>
class persistent_graph {
  private:
    array<Type> re; // [space,time,var], the space dimension should have the same length as graph
    array<Type> mean; // same dimensions as re
    dag<Type> graph;
  public:
    // Constructor
    persistent_graph(
      array<Type> re,
      array<Type> mean,
      dag<Type> graph
    );
    persistent_graph() = default;

    // Accessor methods
    int dim_g() { return graph.size(); }
    int dim_s() { return re.dim(0); }
    int dim_t() { return re.dim(1); }
    int dim_v() { return re.dim(2); }

    array<Type> get_re() { return re; }
    persistent_graph<Type> set_re(array<Type> new_re) { re = new_re; return *this;}

    array<Type> get_mean() { return mean; }
    persistent_graph<Type> set_mean(array<Type> new_mean) { mean = new_mean; return *this;}

    dag<Type> get_graph() { return graph; }

    array<Type> subset_re_by_s(vector<int> idx);
    array<Type> subset_mean_by_s(vector<int> idx);
    persistent_graph_node<Type> operator() (int idx);
    persistent_graph_node<Type> operator() (int idx,int t,int v);
    persistent_graph<Type> set_re_by_to_g(vector<Type> new_re,int idx,int t,int v);
    persistent_graph<Type> set_mean_by_to_g(vector<Type> new_mean,int idx,int t,int v);


    persistent_graph<Type> slice_t(int start, int length);
    persistent_graph<Type> slice_v(int start, int length);
};


// Constructor
template<class Type>
persistent_graph<Type>::persistent_graph(
    array<Type> re,
    array<Type> mean,
    dag<Type> graph
  ) :
  re(re),
  mean(mean),
  graph(graph) {
    // Nothing left to initialize
}


// Don't return a persistent_graph because subsetting the edges/dists will not work.
template<class Type>
array<Type> persistent_graph<Type>::subset_re_by_s(vector<int> idx) {
  array<Type> new_re(re.dim(2),re.dim(1),idx.size()); // [var,time,space]
  for(int s=0; s<idx.size(); s++) {
    new_re.col(s) = re.transpose().col(idx(s));
  }
  new_re = new_re.transpose(); // [space,time,var]

  return new_re;
}


// Don't return a persistent_graph because subsetting the edges/dists will not work.
template<class Type>
array<Type> persistent_graph<Type>::subset_mean_by_s(vector<int> idx) {
  array<Type> new_mean(re.dim(2),re.dim(1),idx.size()); // [var,time,space]
  for(int s=0; s<idx.size(); s++) {
    new_mean.col(s) = mean.transpose().col(idx(s));
  }
  new_mean = new_mean.transpose(); // [space,time,var]

  return new_mean;
}


template<class Type>
persistent_graph_node<Type> persistent_graph<Type>::operator() (int idx) {
  dag_node<Type> node = graph(idx);

  vector<int> node_idx(node.to.size()+node.from.size());
  node_idx << node.to, node.from;
  array<Type> node_re = subset_re_by_s(node_idx);
  array<Type> node_mean = subset_mean_by_s(node_idx);

  persistent_graph_node<Type> pg_node = {node_re,node_mean,node};

  return pg_node;
}


template<class Type>
persistent_graph_node<Type> persistent_graph<Type>::operator() (int idx,int t, int v) {
  persistent_graph_node<Type> node = slice_t(t,1).slice_v(v,1)(idx);

  return node;
}


template<class Type>
persistent_graph<Type> persistent_graph<Type>::set_re_by_to_g(
    vector<Type> new_re,
    int idx,
    int t,
    int v
  ) {
    dag_node<Type> node = operator()(idx).node;
    for(int i=0; i<node.to.size(); i++) {
      re(node.to(i),t,v) = new_re(i);
    }

    return *this;
}


template<class Type>
persistent_graph<Type> persistent_graph<Type>::set_mean_by_to_g(
    vector<Type> new_mean,
    int idx,
    int t,
    int v
  ) {
    dag_node<Type> node = operator()(idx).node;
    for(int i=0; i<node.to.size(); i++) {
      mean(node.to(i),t,v) = new_mean(i);
    }

    return *this;
}



template<class Type>
persistent_graph<Type> persistent_graph<Type>::slice_t(int start,int length) {
  vector<int> permv(3);
  permv << 0, 2, 1; // [space,time,var] <-> [space,var,time]
  array<Type> perm_re = re.perm(permv);
  array<Type> perm_mean = mean.perm(permv);
  array<Type> new_re(re.dim(0),re.dim(2),length); // [space,var,time]
  array<Type> new_mean = new_re;
  for(int t=0; t<length; t++) {
    new_re.col(t) = perm_re.col(t+start);
    new_mean.col(t) = perm_mean.col(t+start);
  }
  new_re = new_re.perm(permv);
  new_mean = new_mean.perm(permv);

  persistent_graph<Type> new_pg(
    new_re,
    new_mean,
    graph
  );

  return new_pg;
}

template<class Type>
persistent_graph<Type> persistent_graph<Type>::slice_v(int start,int length) {
  array<Type> new_re(re.dim(0),re.dim(1),length);
  array<Type> new_mean = new_re;
  for(int v=0; v<length; v++) {
    new_re.col(v) = re.col(v+start);
    new_mean.col(v) = new_mean.col(v+start);
  }

  persistent_graph<Type> new_pg(
    new_re,
    new_mean,
    graph
  );

  return new_pg;
}
