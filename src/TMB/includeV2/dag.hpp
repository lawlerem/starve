template<class Type>
struct dag_node {
  vector<int> to;
  vector<int> from;
  matrix<Type> d;
};

// This is more of a convenience class than it is a structural class
template<class Type>
struct re_dag_node {
  array<Type> re;
  array<Type> mean;
  dag_node<Type> node;
};


template<class Type>
class dag {
  public:
    vector<dag_node<Type> > nodes;
    dag(
      const vector<vector<vector<int> > >& edges,
      const vector<matrix<Type> >& dists
    );
    dag(const vector<dag_node<Type> >& nodes) : nodes{nodes} {};
    dag() {nodes.resize(0);}

    int size() { return nodes.size(); }
    dag_node<Type> operator() (int i) { return nodes(i); }
    dag<Type> segment(int start, int length) const { return dag {nodes.segment(start,length)}; }
    vector<vector<vector<int> > > edges();
    vector<matrix<Type> > dists();
};



template<class Type>
dag<Type>::dag(
    const vector<vector<vector<int> > >& edges,
    const vector<matrix<Type> >& dists
  ) {
    nodes.resizeLike(edges);
    for(int i=0; i<edges.size(); i++) {
      nodes(i) = {edges(i)(0), edges(i)(1), dists(i)};
    }
}

template<class Type>
vector<vector<vector<int> > > dag<Type>::edges() {
  vector<vector<vector<int> > > e(nodes.size());
  for(int i=0; i<e.size(); i++) {
    e(i).resize(2);
    e(i)(0) = nodes(i).to;
    e(i)(1) = nodes(i).from;
  }
  return e;
}

template<class Type>
vector<matrix<Type> > dag<Type>::dists() {
  vector<matrix<Type> > d(nodes.size());
  for(int i=0; i<d.size(); i++) {
    d(i) = nodes(i).d;
  }
  return d;
}
