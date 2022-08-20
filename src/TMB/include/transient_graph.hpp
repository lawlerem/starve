template<class Type>
class transient_graph {
  public:
    vector<array<Type> > re; // [time[idx,var] ]
    vector<array<Type> > mean; // [time[idx,var] ]
    vector<dag<Type> > graph; // [time[graph] ]
    transient_graph(
      array<Type> all_re, // [idx,var] can't do const-reference
      array<Type> all_mean, // [idx,var] can't do const-reference
      const dag<Type>& all_graph,
      const vector<int>& t,
      int tmax
    );
    transient_graph(
      const vector<array<Type> >& re, // [time[idx,var] ]
      const vector<array<Type> >& mean,
      const vector<dag<Type> >& graph // [time[graph] ]
    ) : re{re}, mean{mean}, graph{graph} {};

    // Accessor methods
    int dim_g(int t) { return graph(t).size(); }
    int dim_s(int t) { return re(t).dim(0); }
    int dim_t() { return graph.size(); }
    int dim_v() { return re(0).dim(1); }

    array<Type> get_re();
    array<Type> get_mean();
    vector<dag<Type> > get_graph() { return graph; }

    re_dag_node<Type> operator() (
      int idx,
      int t,
      persistent_graph<Type>& pg
    );

    re_dag_node<Type> operator() (
      int idx,
      int t,
      int v,
      persistent_graph<Type>& pg
    );

    transient_graph<Type> set_re_by_to_g(
      const vector<Type>& new_re,
      int idx,
      int t,
      int v,
      persistent_graph<Type>& pg
    );

    transient_graph<Type> set_mean_by_to_g(
      const vector<Type>& new_mean,
      int idx,
      int t,
      int v,
      persistent_graph<Type>& pg
    );

    transient_graph<Type> slice_t(int start, int length);
    transient_graph<Type> slice_v(int start, int length);
};


// Constructor
template<class Type>
transient_graph<Type>::transient_graph(
    array<Type> all_re,
    array<Type> all_mean,
    const dag<Type>& all_graph,
    const vector<int>& t,
    int tmax) {
  re.resize(tmax + 1);
  mean.resize(tmax + 1);
  graph.resize(tmax + 1);
  for(int i = 0; i <= tmax; i++) {
    vector<int> t_segment = get_time_segment(t, i);
    re(i) = array<Type>(t_segment(1), all_re.cols());
    mean(i) = array<Type>(t_segment(1), all_mean.cols());
    if( t_segment(1) > 0 ) {
      re(i) = matrix_row_segment(all_re.matrix(), t_segment(0), t_segment(1)).array();
      mean(i) = matrix_row_segment(all_mean.matrix(),t_segment(0), t_segment(1)).array();
    }

    graph(i) = dag<Type>(all_graph.segment(t_segment(0), t_segment(1)));
  }
}


template<class Type>
array<Type> transient_graph<Type>::get_re() {
  int n_re = 0;
  for(int i = 0; i < re.size(); i++) {
    n_re += re(i).dim(0);
  }

  array<Type> ans(n_re, re(0).dim(1)); // [idx,var]
  int cntr = 0;
  for(int i = 0; i < re.size(); i++) {
    for(int j = 0; j < re(i).dim(0); j++) {
      for(int v = 0; v < re(i).dim(1); v++) {
        ans(cntr, v) = re(i)(j, v);
      }
      cntr++;
    }
  }

  return ans;
}

template<class Type>
array<Type> transient_graph<Type>::get_mean() {
  int n_mean = 0;
  for(int i = 0; i < mean.size(); i++) {
    n_mean += mean(i).dim(0);
  }

  array<Type> ans(n_mean, mean(0).dim(1)); // [idx,var]
  int cntr = 0;
  for(int i = 0; i < mean.size(); i++) {
    for(int j = 0; j < mean(i).dim(0); j++) {
      for(int v = 0; v < mean(i).dim(1); v++) {
        ans(cntr, v) = mean(i)(j, v);
      }
      cntr++;
    }
  }

  return ans;
}




template<class Type>
re_dag_node<Type> transient_graph<Type>::operator() (
    int idx,
    int t,
    persistent_graph<Type>& pg) {
  int n = graph(t)(idx).to.size() + graph(t)(idx).from.size();
  re_dag_node<Type> node {
    array<Type>(n, re(0).dim(1)),
    array<Type>(n, mean(0).dim(1)),
    graph(t)(idx)
  };

  for(int i = 0; i < node.node.to.size(); i++) {
    for(int v = 0; v < re(0).dim(1); v++) {
      node.re(i, v) = re(t)(node.node.to(i), v);
      node.mean(i, v) = mean(t)(node.node.to(i), v);
    }
  }
  for(int i = 0; i < node.node.from.size(); i++) {
    for(int v = 0; v < re(0).dim(1); v++) {
      array<Type> from_re = pg.subset_re_by_s(node.node.from);
      array<Type> from_mean = pg.subset_mean_by_s(node.node.from);
      node.re(i + node.node.to.size(), v) = from_re(i, t, v);
      node.mean(i + node.node.to.size(), v) = from_mean(i, t, v);
    }
  }

  return node;
}

template<class Type>
re_dag_node<Type> transient_graph<Type>::operator() (
    int idx,
    int t,
    int v,
    persistent_graph<Type>& pg) {
  int n = graph(t)(idx).to.size() + graph(t)(idx).from.size();
  re_dag_node<Type> node {
    array<Type>(n, 1),
    array<Type>(n, 1),
    graph(t)(idx)
  };

  for(int i = 0; i < node.node.to.size(); i++) {
    node.re(i, 0) = re(t)(node.node.to(i), v);
    node.mean(i, 0) = mean(t)(node.node.to(i), v);
  }
  for(int i = 0; i < node.node.from.size(); i++) {
    array<Type> from_re = pg.subset_re_by_s(node.node.from);
    array<Type> from_mean = pg.subset_mean_by_s(node.node.from);
    node.re(i + node.node.to.size(), 0) = from_re(i, t, v);
    node.mean(i + node.node.to.size(), 0) = from_mean(i, t, v);
  }

  return node;
}



template<class Type>
transient_graph<Type> transient_graph<Type>::set_re_by_to_g(
    const vector<Type>& new_re,
    int idx,
    int t,
    int v,
    persistent_graph<Type>& pg) {
  dag_node<Type> node = operator()(idx, t, v, pg).node;
  for(int i = 0; i < node.to.size(); i++) {
    re(t)(node.to(i), v) = new_re(i);
  }

  return *this;
}

template<class Type>
transient_graph<Type> transient_graph<Type>::set_mean_by_to_g(
    const vector<Type>& new_mean,
    int idx,
    int t,
    int v,
    persistent_graph<Type>& pg) {
  dag_node<Type> node = operator()(idx, t, v, pg).node;
  for(int i = 0; i < node.to.size(); i++) {
    mean(t)(node.to(i), v) = new_mean(i);
  }

  return *this;
}




template<class Type>
transient_graph<Type> transient_graph<Type>::slice_t(int start, int length) {
  return transient_graph<Type> {
    re.segment(start, length),
    mean.segment(start, length),
    graph.segment(start, length)
  };
}

template<class Type>
transient_graph<Type> transient_graph<Type>::slice_v(int start, int length) {
  vector<array<Type> > new_re(re.size());
  vector<array<Type> > new_mean(mean.size());
  for(int i = 0; i < new_re.size(); i++) {
    new_re(i) = array<Type>(re(i).rows(), length);
    new_mean(i) = array<Type>(mean(i).rows(), length);
    for(int v = 0; v < length; v++) {
      new_re(i).col(v) = re(i).col(v + start);
      new_mean(i).col(v) = mean(i).col(v + start);
    }
  }

  transient_graph<Type> new_tg(
    new_re,
    new_mean,
    graph
  );

  return new_tg;
}
