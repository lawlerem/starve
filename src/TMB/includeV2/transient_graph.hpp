template<class Type>
struct transient_graph_node {
  array<Type> re;
  dag_node<Type> node;
};

template<class Type>
class transient_graph {
  private:
    vector<array<Type> > re; // [time[idx,var] ]
    vector<dag<Type> > graph; // [time[graph] ]
  public:
    transient_graph(
      array<Type> all_re, // [idx,var]
      dag<Type> all_graph,
      vector<int> t,
      int tmax
    );
    transient_graph(
      vector<array<Type> > re, // [time[idx,var] ]
      vector<dag<Type> > graph // [time[graph] ]
    );
    transient_graph() = default;

    // Accessor methods
    array<Type> get_re();
    transient_graph_node<Type> operator() (
      int t,
      int idx,
      persistent_graph<Type> pg
    );
    transient_graph<Type> slice_t(int start, int length);
    transient_graph<Type> slice_v(int start, int length);
};


// Constructor
template<class Type>
transient_graph<Type>::transient_graph(
    array<Type> all_re,
    dag<Type> all_graph,
    vector<int> t,
    int tmax
  ) {
  re.resize(tmax+1);
  graph.resize(tmax+1);
  for(int i=0; i<=tmax; i++) {
    vector<int> t_segment = get_time_segment(t,i);
    re(i) = array<Type>(t_segment(1),all_re.cols());
    re(i) = matrix_row_segment(all_re.matrix(),t_segment(0),t_segment(1)).array();

    graph(i) = dag<Type>(all_graph.segment(t_segment(0),t_segment(1)));
  }
}

template<class Type>
transient_graph<Type>::transient_graph(
    vector<array<Type> > re,
    vector<dag<Type> > graph
  ) :
  re(re),
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
transient_graph_node<Type> transient_graph<Type>::operator() (
    int t,
    int idx,
    persistent_graph<Type> pg
  ) {
    dag_node<Type> node = graph(t)(idx);

    array<Type> node_re(re(0).cols(),node.to.size()+node.from.size()); // [var,idx]
    for(int i=0; i<node.to.size(); i++) {
      node_re.col(i) = re(t).transpose().col(node.to(i)); // Gets node.to(i) row
    }
    for(int i=0; i<node.from.size(); i++) {
      array<Type> from_re = pg.subset_re_by_s(node.from);
      vector<int> permv(3);
      permv << 2, 0, 1; // [space,time,var] <-> [var,space,time]
      node_re.col(i+node.to.size()) = from_re.perm(permv).col(t).col(i);
    }
    // node_re = node_re.transpose();

    transient_graph_node<Type> tg_node = {node_re.transpose(),node};

    return tg_node;
}


template<class Type>
transient_graph<Type> transient_graph<Type>::slice_t(int start,int length) {
  transient_graph<Type> new_tg(
    re.segment(start,length),
    graph.segment(start,length)
  );

  return(new_tg);
}

template<class Type>
transient_graph<Type> transient_graph<Type>::slice_v(int start,int length) {
  vector<array<Type> > new_re(re.size());
  for(int i=0; i<new_re.size(); i++) {
    new_re(i) = array<Type>(re(i).rows(),length);
    for(int v=0; v<length; v++) {
      new_re(i).col(v) = re(i).col(v+start);
    }
  }

  transient_graph<Type> new_tg(
    new_re,
    graph
  );

  return new_tg;
}
