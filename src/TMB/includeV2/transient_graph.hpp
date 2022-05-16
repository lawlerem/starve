template<class Type>
class transient_graph {
  private:
    vector<array<Type> > re; // [time[idx,var] ]
    vector<array<Type> > mean; // [time[idx,var] ]
    vector<dag<Type> > graph; // [time[graph] ]
  public:
    transient_graph(
      array<Type> all_re, // [idx,var]
      array<Type> all_mean, // [idx,var]
      dag<Type> all_graph,
      vector<int> t,
      int tmax
    );
    transient_graph(
      vector<array<Type> > re, // [time[idx,var] ]
      vector<array<Type> > mean,
      vector<dag<Type> > graph // [time[graph] ]
    );
    transient_graph() = default;

    // Accessor methods
    int dim_g(int t) { return graph(t).size(); }
    int dim_s(int t) { return re(t).dim(0); }
    int dim_t() { return graph.size(); }
    int dim_v() { return re(0).dim(1); }

    array<Type> get_re();
    array<Type> get_mean();
    re_dag_node<Type> operator() (
      int idx,
      int t,
      persistent_graph<Type> pg
    );
    re_dag_node<Type> operator() (
      int idx,
      int t,
      int v,
      persistent_graph<Type> pg
    ) { return slice_v(v,1)(idx,t,pg.slice_v(v,1)); }
    transient_graph<Type> set_re_by_to_g(
      vector<Type> new_re,
      int idx,
      int t,
      int v,
      persistent_graph<Type> pg
    );
    transient_graph<Type> set_mean_by_to_g(
      vector<Type> new_mean,
      int idx,
      int t,
      int v,
      persistent_graph<Type> pg
    );

    transient_graph<Type> slice_t(int start, int length);
    transient_graph<Type> slice_v(int start, int length);
};


// Constructor
template<class Type>
transient_graph<Type>::transient_graph(
    array<Type> all_re,
    array<Type> all_mean,
    dag<Type> all_graph,
    vector<int> t,
    int tmax
  ) {
  re.resize(tmax+1);
  mean.resize(tmax+1);
  graph.resize(tmax+1);
  for(int i=0; i<=tmax; i++) {
    vector<int> t_segment = get_time_segment(t,i);
    re(i) = array<Type>(t_segment(1),all_re.cols());
    re(i) = matrix_row_segment(all_re.matrix(),t_segment(0),t_segment(1)).array();

    mean(i) = array<Type>(t_segment(1),all_mean.cols());
    mean(i) = matrix_row_segment(all_mean.matrix(),t_segment(0),t_segment(1)).array();

    graph(i) = dag<Type>(all_graph.segment(t_segment(0),t_segment(1)));
  }
}

template<class Type>
transient_graph<Type>::transient_graph(
    vector<array<Type> > re,
    vector<array<Type> > mean,
    vector<dag<Type> > graph
  ) :
  re(re),
  mean(mean),
  graph(graph) {
    // Nothing left to initialize
}


template<class Type>
array<Type> transient_graph<Type>::get_re() {
  int n_re = 0;
  for(int i=0; i<re.size(); i++) {
    n_re += re(i).rows();
  }
  array<Type> ans(re(0).cols(),n_re); // [var,idx

  int cntr=0;
  for(int i=0; i<re.size(); i++) {
    if( re(i).rows() == 0 ) {continue;}
    for(int j=0; j<re(i).rows(); j++) {
      ans.col(cntr) = re(i).transpose().col(j);
      cntr++;
    }
  }
  ans = ans.transpose();

  return ans;
}

template<class Type>
array<Type> transient_graph<Type>::get_mean() {
  int n_mean = 0;
  for(int i=0; i<mean.size(); i++) {
    n_mean += mean(i).rows();
  }
  array<Type> ans(mean(0).cols(),n_mean); // [var,idx

  int cntr=0;
  for(int i=0; i<mean.size(); i++) {
    if( mean(i).rows() == 0 ) {continue;}
    for(int j=0; j<re(i).rows(); j++) {
      ans.col(cntr) = mean(i).transpose().col(j);
      cntr++;
    }
  }
  ans = ans.transpose();

  return ans;
}


template<class Type>
re_dag_node<Type> transient_graph<Type>::operator() (
    int idx,
    int t,
    persistent_graph<Type> pg
  ) {
    dag_node<Type> node = graph(t)(idx);

    array<Type> node_re(re(0).cols(),node.to.size()+node.from.size()); // [var,idx]
    array<Type> node_mean(mean(0).cols(),node.to.size()+node.from.size()); // [var,idx]
    for(int i=0; i<node.to.size(); i++) {
      node_re.col(i) = re(t).transpose().col(node.to(i)); // Gets node.to(i) row
      node_mean.col(i) = mean(t).transpose().col(node.to(i));
    }
    for(int i=0; i<node.from.size(); i++) {
      array<Type> from_re = pg.subset_re_by_s(node.from);
      array<Type> from_mean = pg.subset_mean_by_s(node.from);
      vector<int> permv(3);
      permv << 2, 0, 1; // [space,time,var] <-> [var,space,time]
      node_re.col(i+node.to.size()) = from_re.perm(permv).col(t).col(i);
      node_mean.col(i+node.to.size()) = from_mean.perm(permv).col(t).col(i);
    }

    re_dag_node<Type> tg_node = {node_re.transpose(),node_mean.transpose(),node};

    return tg_node;
}


template<class Type>
transient_graph<Type> transient_graph<Type>::set_re_by_to_g(
    vector<Type> new_re,
    int idx,
    int t,
    int v,
    persistent_graph<Type> pg
  ) {
    dag_node<Type> node = operator()(idx,t,v,pg).node;
    for(int i=0; i<node.to.size(); i++) {
      re(t)(node.to(i),v) = new_re(i);
    }

    return *this;
}

template<class Type>
transient_graph<Type> transient_graph<Type>::set_mean_by_to_g(
    vector<Type> new_mean,
    int idx,
    int t,
    int v,
    persistent_graph<Type> pg
  ) {
    dag_node<Type> node = operator()(idx,t,v,pg).node;
    for(int i=0; i<node.to.size(); i++) {
      mean(t)(node.to(i),v) = new_mean(i);
    }

    return *this;
}




template<class Type>
transient_graph<Type> transient_graph<Type>::slice_t(int start,int length) {
  transient_graph<Type> new_tg(
    re.segment(start,length),
    mean.segment(start,length),
    graph.segment(start,length)
  );

  return(new_tg);
}

template<class Type>
transient_graph<Type> transient_graph<Type>::slice_v(int start,int length) {
  vector<array<Type> > new_re(re.size());
  vector<array<Type> > new_mean(mean.size());
  for(int i=0; i<new_re.size(); i++) {
    new_re(i) = array<Type>(re(i).rows(),length);
    new_mean(i) = array<Type>(mean(i).rows(),length);
    for(int v=0; v<length; v++) {
      new_re(i).col(v) = re(i).col(v+start);
      new_mean(i).col(v) = mean(i).col(v+start);
    }
  }

  transient_graph<Type> new_tg(
    new_re,
    new_mean,
    graph
  );

  return new_tg;
}
