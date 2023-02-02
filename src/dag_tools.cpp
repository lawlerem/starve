#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]


/*
Get the "to" and "from" vertices from a graph node
*/

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


/*
Subroutines for sorting vectors / matrices
*/

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




/*
Create persistent, transient, and prediction graphs from a distance matrix
*/

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
    Rcpp::Named("from") = Eigen::Matrix<int, 0, 1>::Constant(0)
  );
  for(int i = 1; i < edge_list.size(); i++) {
    edge_list[i] = Rcpp::List::create(
      Rcpp::Named("to") = Eigen::Matrix<int, 1, 1>::Constant(i + n_neighbours - 1),
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


// [[Rcpp::export("dist_to_tg_dag")]]
SEXP dist_to_tg_dag(
    const Eigen::Map<Eigen::MatrixXd> &d,
    const Eigen::Map<Eigen::MatrixXd> &pg_d,
    const int n_neighbours) {
  std::vector<SEXP> edge_list(d.rows());
  for(int i = 0; i < d.rows(); i++) {
    edge_list[i] = Rcpp::List::create(
      Rcpp::Named("to") = Eigen::Matrix<int, 1, 1>::Constant(i),
      Rcpp::Named("from") = lowest_k(d.row(i), n_neighbours)
    );
  }

  std::vector<Eigen::MatrixXd> dist_list(d.rows());
  for(int i = 0; i < dist_list.size(); i++) {
    Eigen::VectorXi nodes = from_list(edge_list[i]);
    dist_list[i].resize(nodes.size() + 1, nodes.size() + 1);
    for(int r = 0; r < nodes.size() + 1; r++) {
      for(int c = 0; c < nodes.size() + 1; c++) {
        if( (r == 0) & (c == 0) ) {
          // tg node to itself
          dist_list[i](r, c) = 0.0;
        } else if( (r == 0) & (c > 0) ) {
          // tg node (r) to pg node (c)
          dist_list[i](r, c) = d(i, nodes(c - 1));
        } else if( (r > 0) & (c == 0) ) {
          // pg node (r) to tg node (c)
          dist_list[i](r, c) = d(i, nodes(r - 1));
        } else {
          // pg node (r) to tg node (c)
          dist_list[i](r, c) = pg_d(nodes(r - 1), nodes(c - 1));
        }
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("edge_list") = edge_list,
    Rcpp::Named("dist_list") = dist_list
  );
}


// [[Rcpp::export("dist_to_pred_dag")]]
SEXP dist_to_pred_dag(
    const Eigen::Map<Eigen::MatrixXd> &d,
    const Eigen::Map<Eigen::MatrixXd> &g_d,
    const Eigen::MatrixXi &node_alignment,
    const int n_neighbours) {
  std::vector<SEXP> edge_list(d.rows());
  std::vector<Eigen::VectorXi> from_lists(d.rows());
  for(int i = 0; i < d.rows(); i++) {
    from_lists[i] = lowest_k(d.row(i), n_neighbours);
    Eigen::VectorXi aligned_from = from_lists[i];
    for(int j = 0; j < aligned_from.size(); j++) {
      aligned_from(j) = node_alignment(i, aligned_from(j));
    }
    edge_list[i] = Rcpp::List::create(
      Rcpp::Named("to") = Eigen::Matrix<int, 1, 1>::Constant(i),
      // Change "from" later using node_alignment
      Rcpp::Named("from") = aligned_from
    );
  }

  std::vector<Eigen::MatrixXd> dist_list(d.rows());
  for(int i = 0; i < dist_list.size(); i++) {
    Eigen::VectorXi nodes = from_lists[i];
    dist_list[i].resize(nodes.size() + 1, nodes.size() + 1);
    for(int r = 0; r < nodes.size() + 1; r++) {
      for(int c = 0; c < nodes.size() + 1; c++) {
        if( (r == 0) & (c == 0) ) {
          // tg node to itself
          dist_list[i](r, c) = 0.0;
        } else if( (r == 0) & (c > 0) ) {
          // tg node (r) to pg node (c)
          dist_list[i](r, c) = d(i, nodes(c - 1));
        } else if( (r > 0) & (c == 0) ) {
          // pg node (r) to tg node (c)
          dist_list[i](r, c) = d(i, nodes(r - 1));
        } else {
          // pg node (r) to tg node (c)
          dist_list[i](r, c) = g_d(nodes(r - 1), nodes(c - 1));
        }
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("edge_list") = edge_list,
    Rcpp::Named("dist_list") = dist_list
  );
}
