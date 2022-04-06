template<class Type>
struct dag_node {
  vector<int> to;
  vector<int> from;
  matrix<Type> d;
};

template<class Type>
class dag {
  private:
    vector<dag_node<Type> > nodes;
  public:
    dag(
      vector<vector<vector<int> > > edges,
      vector<matrix<Type> > dists
    );
    dag(vector<dag_node<Type> > nodes) : nodes(nodes) {};
    dag() = default;

    int size() { return nodes.size(); }
    vector<dag_node<Type> > get_nodes() { return nodes; }
    dag_node<Type> operator() (int i) { return nodes(i); }
    dag<Type> segment(int start, int length) { return dag(nodes.segment(start,length)); }
    vector<vector<vector<int> > > edges();
    vector<matrix<Type> > dists();
};



template<class Type>
dag<Type>::dag(
    vector<vector<vector<int> > > edges,
    vector<matrix<Type> > dists
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
