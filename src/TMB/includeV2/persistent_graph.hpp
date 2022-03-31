template<class Type>
struct persistent_graph_node {
  array<Type> re;
  dag_node<Type> node;
};

// A class to hold the random effects and graph structure for the persistent random effects
template<class Type>
class persistent_graph {
  private:
    array<Type> re; // [space,time,var], the space dimension should have the same length as graph
    dag<Type> graph;
  public:
    // Constructor
    persistent_graph(
      array<Type> re,
      dag<Type> graph
    );
    persistent_graph() = default;

    // Accessor methods
    array<Type> get_re() { return re; }
    dag<Type> get_graph() { return graph; }
    array<Type> subset_re_by_s(vector<int> idx);
    persistent_graph_node<Type> operator() (int idx);
    persistent_graph_node<Type> operator() (int idx,int t,int v);
    persistent_graph<Type> slice_t(int start, int length);
    persistent_graph<Type> slice_v(int start, int length);
};


// Constructor
template<class Type>
persistent_graph<Type>::persistent_graph(
    array<Type> re,
    dag<Type> graph
  ) :
  re(re),
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


template<class Type>
persistent_graph_node<Type> persistent_graph<Type>::operator() (int idx) {
  dag_node<Type> node = graph(idx);

  vector<int> node_re_idx(node.to.size()+node.from.size());
  node_re_idx << node.to, node.from;
  array<Type> node_re = subset_re_by_s(node_re_idx);

  persistent_graph_node<Type> pg_node = {node_re,node};

  return pg_node;
}


template<class Type>
persistent_graph_node<Type> persistent_graph<Type>::operator() (int idx,int t, int v) {
  persistent_graph_node<Type> node = slice_t(t,1).slice_v(v,1)(idx);

  return node;
}


template<class Type>
persistent_graph<Type> persistent_graph<Type>::slice_t(int start,int length) {
  vector<int> permv(3);
  permv << 0, 2, 1; // [space,time,var] <-> [space,var,time]
  array<Type> perm_re = re.perm(permv);
  array<Type> new_re(re.dim(0),re.dim(2),length); // [space,var,time]
  for(int t=0; t<length; t++) {
    new_re.col(t) = perm_re.col(t+start);
  }
  new_re = new_re.perm(permv);

  persistent_graph<Type> new_pg(
    new_re,
    graph
  );

  return new_pg;
}

template<class Type>
persistent_graph<Type> persistent_graph<Type>::slice_v(int start,int length) {
  array<Type> new_re(re.dim(0),re.dim(1),length);
  for(int v=0; v<length; v++) {
    new_re.col(v) = re.col(v+start);
  }

  persistent_graph<Type> new_pg(
    new_re,
    graph
  );

  return new_pg;
}
