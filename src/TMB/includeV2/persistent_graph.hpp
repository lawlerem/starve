// A class to hold the random effects and graph structure for the persistent random effects
template<class Type>
class persistent_graph {
  public:
    array<Type> re; // [space,time,var], the space dimension should have the same length as graph
    array<Type> mean; // same dimensions as re
    dag<Type> graph;

    // Constructor
    persistent_graph(
      const array<Type>& re,
      const array<Type>& mean,
      const dag<Type>& graph
    ) : re{re}, mean{mean}, graph{graph} {};

    // Accessor methods
    int dim_g() { return graph.size(); }
    int dim_s() { return re.dim(0); }
    int dim_t() { return re.dim(1); }
    int dim_v() { return re.dim(2); }

    array<Type> subset_re_by_s(const vector<int>& idx);
    array<Type> subset_mean_by_s(const vector<int>& idx);

    // operator(idx) subsets by the idx'th graph node
    re_dag_node<Type> operator() (int idx); // Why not return a persistent_graph? Because the nodes wont be complete
    re_dag_node<Type> operator() (int idx,int t);
    re_dag_node<Type> operator() (int idx,int t,int v);
    persistent_graph<Type> set_re_by_to_g(const vector<Type>& new_re,int idx,int t,int v);
    persistent_graph<Type> set_mean_by_to_g(const vector<Type>& new_mean,int idx,int t,int v);


    persistent_graph<Type> slice_t(int start, int length);
    persistent_graph<Type> slice_v(int start, int length);
};



// Don't return a persistent_graph because subsetting the edges/dists will not work.
template<class Type>
array<Type> persistent_graph<Type>::subset_re_by_s(const vector<int>& idx) {
  array<Type> new_re(idx.size(),re.dim(1),re.dim(2)); // [var,time,space]
  for(int s=0; s<idx.size(); s++) {
    for(int t=0; t<re.dim(1); t++) {
      for(int v=0; v<re.dim(2); v++) {
        new_re(s,t,v) = re(idx(s),t,v);
      }
    }
  }

  return new_re;
}


// Don't return a persistent_graph because subsetting the edges/dists will not work.
template<class Type>
array<Type> persistent_graph<Type>::subset_mean_by_s(const vector<int>& idx) {
  array<Type> new_mean(idx.size(),mean.dim(1),mean.dim(2));
  for(int s=0; s<idx.size(); s++) {
    for(int t=0; t<re.dim(1); t++) {
      for(int v=0; v<re.dim(2); v++) {
        new_mean(s,t,v) = mean(idx(s),t,v);
      }
    }
  }

  return new_mean;
}


template<class Type>
re_dag_node<Type> persistent_graph<Type>::operator() (int idx) {
  dag_node<Type> node = graph(idx);

  vector<int> node_idx(node.to.size()+node.from.size());
  node_idx << node.to, node.from;
  array<Type> node_re = subset_re_by_s(node_idx);
  array<Type> node_mean = subset_mean_by_s(node_idx);

  return re_dag_node<Type> {node_re, node_mean, node};
}

template<class Type>
re_dag_node<Type> persistent_graph<Type>::operator() (int idx,int t) {
  return slice_t(t,1)(idx);
}

template<class Type>
re_dag_node<Type> persistent_graph<Type>::operator() (int idx,int t, int v) {
  return slice_t(t,1).slice_v(v,1)(idx);
}


template<class Type>
persistent_graph<Type> persistent_graph<Type>::set_re_by_to_g(
    const vector<Type>& new_re,
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
    const vector<Type>& new_mean,
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
  array<Type> new_re(re.dim(0),length,re.dim(2));
  array<Type> new_mean(mean.dim(0),length,mean.dim(2));
  for(int s=0; s<re.dim(0); s++) {
    for(int t=0; t<length; t++) {
      for(int v=0; v<re.dim(2); v++) {
        new_re(s,t,v) = re(s,t+start,v);
        new_mean(s,t,v) = mean(s,t+start,v);
      }
    }
  }

  return persistent_graph<Type> {new_re, new_mean, graph};
}

template<class Type>
persistent_graph<Type> persistent_graph<Type>::slice_v(int start,int length) {
  array<Type> new_re(re.dim(0),re.dim(1),length);
  array<Type> new_mean(mean.dim(0),mean.dim(1),length);
  for(int s=0; s<re.dim(0); s++) {
    for(int t=0; t<re.dim(1); t++) {
      for(int v=0; v<length; v++) {
        new_re(s,t,v) = re(s,t,v+start);
        new_mean(s,t,v) = mean(s,t,v+start);
      }
    }
  }

  return persistent_graph<Type> {new_re, new_mean, graph};
}
