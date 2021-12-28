test_that("Lowest k of vector",{
  expect_equal(.rcpp_lowest_k(1:10,5),1:5-1)
  expect_equal(.rcpp_lowest_k(c(1,3.5,-1.2,4,2),1),c(3,1,5,2,4)[1]-1)
  expect_equal(.rcpp_lowest_k(c(1,3.5,-1.2,4,2),2),c(3,1,5,2,4)[1:2]-1)
  expect_equal(.rcpp_lowest_k(c(1,3.5,-1.2,4,2),3),c(3,1,5,2,4)[1:3]-1)
  expect_equal(.rcpp_lowest_k(c(1,3.5,-1.2,4,2),4),c(3,1,5,2,4)[1:4]-1)
  expect_equal(.rcpp_lowest_k(c(1,3.5,-1.2,4,2),5),c(3,1,5,2,4)[1:5]-1)
  expect_equal(.rcpp_lowest_k(1:5,0),integer(0))
  expect_error(.rcpp_lowest_k(1:10,11),"k is greater")
  expect_error(.rcpp_lowest_k(c("a","b","c"),2),"Not compatible")
})

test_that("Order distance matrix",{
  expect_equal(.rcpp_order_d_matrix(rbind(c(1,2,3),c(4,5,6),c(7,8,9))),c(1,2,3)-1)
  expect_equal(.rcpp_order_d_matrix(rbind(c(0,1,3),c(1,0,4),c(3,4,0))),c(1,2,3)-1)
  expect_equal(.rcpp_order_d_matrix(rbind(c(0,3,1),c(3,0,4),c(4,3,0))),c(1,3,2)-1)

  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(1:5))),1:5-1)
  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(5:1))),1:5-1)
  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(c(1,3,4,5,2)))),c(1,5,2,3,4)-1)
  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(c(1,3,5,2,4)))),c(1,4,2,5,3)-1)
  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(c(3,2,4,5,1)))),c(1,2,3,4,5)-1)
  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(c(3,2,5,1,1,4)))),c(1,2,4,5,6,3)-1)
  expect_equal(.rcpp_order_d_matrix(as.matrix(dist(c(3,2,5,1,1,4,-1)))),c(1,2,4,5,6,3,7)-1)

  expect_error(.rcpp_order_d_matrix(rbind(c("a","b"),c("c","d"))),"Not compatible")
})

test_that("Distance matrix to dag",{
  d<- unname(as.matrix(dist(1:5)))
  order<- .rcpp_order_d_matrix(d)
  d<- d[order+1,order+1]
  e<- list(
    order = order,
    edge_list = list(
      list(to = 0:1,from = integer(0)),
      list(to = 2,from = 1:0),
      list(to = 3,from = 2:1),
      list(to = 4,from = 3:2)
    ),
    dist_list = list(
      d[1:2,1:2],
      d[1:3,1:3],
      d[2:4,2:4],
      d[3:5,3:5]
    )
  )
  expect_equal(.rcpp_dist_to_dag(d,2),e)

  d<- unname(as.matrix(dist(c(3,2,5,1,1,4,-1))))
  # 3 2 1 1 4 5 -1
  order<- .rcpp_order_d_matrix(d)
  d<- d[order+1,order+1]
  e<- list(
    order = .rcpp_order_d_matrix(d),
    edge_list = list(
      list(to = 0:2,from = integer(0)),
      list(to = 3,from = c(2,1,0)),
      list(to = 4,from = c(0,1,2)),
      list(to = 5,from = c(4,0,1)),
      list(to = 6,from = c(2,3,1))
    ),
    dist_list = list(
      d[c(1,2,3),c(1,2,3)],
      d[c(4,3,2,1),c(4,3,2,1)],
      d[c(5,1,2,3),c(5,1,2,3)],
      d[c(6,5,1,2),c(6,5,1,2)],
      d[c(7,3,4,2),c(7,3,4,2)]
    )
  )
  expect_equal(.rcpp_dist_to_dag(d,3),e)
})
