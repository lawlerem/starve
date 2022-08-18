#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

Eigen::VectorXi from_list(Rcpp::List edge_list) {
  return Rcpp::as<Eigen::VectorXi>(
    edge_list["from"]
  );
}
Eigen::VectorXi to_list(Rcpp::List edge_list) {
  return Rcpp::as<Eigen::VectorXi>(
    edge_list["to"]
  );
}


// [[Rcpp::export("order_d_matrix")]]
Eigen::VectorXi order_d_matrix(Eigen::MatrixXd &d) {
  Eigen::VectorXi order = Eigen::VectorXi::LinSpaced(
    d.rows(),
    0,
    d.rows() - 1
  );
  int minParent, minChild;
  for(int i = 1; i < d.rows(); i++) {
    // Find least distance
    d.topRightCorner(i, d.cols() - i).minCoeff(&minParent, &minChild);
    minChild += i;
    // Put closest node next
    d.row(i).swap(d.row(minChild));
    d.col(i).swap(d.col(minChild));
    std::iter_swap(order.data() + i, order.data() + minChild);
  }
  return order;
}




struct refSorter {
  refSorter(const Eigen::VectorXd &d) : d_(d) {}
  bool operator () (const int a, const int b) {
    return d_(a) < d_(b);
  }
  const Eigen::VectorXd d_;
};


// [[Rcpp::export("lowest_k")]]
Eigen::VectorXi lowest_k(const Eigen::VectorXd &d, const int k) {
  if( k == 0 ) {
    Eigen::VectorXi v(0);
    return v;
  } else if( k > d.size() ) {
    Rcpp::stop("k is greater than size of vector d.");
  }

  Eigen::VectorXi ind = Eigen::VectorXi::LinSpaced(
    d.size(),
    0,
    d.size() - 1
  );
  std::partial_sort(
    ind.data(),
    ind.data() + k,
    ind.data() + ind.size(),
    refSorter(d)
  );
  return ind.segment(0, k);
}

// [[Rcpp::export("dist_to_dag")]]
SEXP dist_to_dag(const Eigen::Map<Eigen::MatrixXd> &d, const int n_neighbours) {
  Eigen::MatrixXd sorted_d = d;
  // Returns order, and sorts ordered_d
  Eigen::VectorXi order = order_d_matrix(sorted_d);

  // Not equal to d.cols() because first graph node is a K_n graph
  // using up the first n_neighbours locations
  int dagSize = d.cols() - n_neighbours + 1;

  std::vector<SEXP> edge_list(dagSize);
  edge_list[0] = Rcpp::List::create(
    Rcpp::Named("to") = Eigen::VectorXi::LinSpaced(
      n_neighbours,
      0,
      n_neighbours - 1
    ),
    Rcpp::Named("from") = Eigen::Matrix<int,0,1>::Constant(0)
  );
  for(int i = 1; i < edge_list.size(); i++) {
    edge_list[i] = Rcpp::List::create(
      Rcpp::Named("to") = Eigen::Matrix<int,1,1>::Constant(i + n_neighbours - 1),
      Rcpp::Named("from") = lowest_k(
        sorted_d.col(i + n_neighbours - 1).head(i + n_neighbours - 1),
        n_neighbours
      )
    );
  }

  std::vector<Eigen::MatrixXd> dist_list(dagSize);
  for(int i = 0; i < dist_list.size(); i++) {
    Eigen::VectorXi all_nodes(
      to_list(edge_list[i]).size() + from_list(edge_list[i]).size()
    );
    all_nodes << to_list(edge_list[i]), from_list(edge_list[i]);
    dist_list[i].resize(all_nodes.size(), all_nodes.size());
    for(int j = 0; j < all_nodes.size(); j++) {
      for(int jj = 0; jj < all_nodes.size(); jj++) {
        dist_list[i](j, jj) = sorted_d(all_nodes(j), all_nodes(jj));
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("order") = order,
    Rcpp::Named("edge_list") = edge_list,
    Rcpp::Named("dist_list") = dist_list
  );
}
