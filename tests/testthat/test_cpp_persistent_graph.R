library(staRVe)
library(testthat)

points<- sf::st_as_sf(as.data.frame(rbind(
  # persistent graph nodes
  c(0,0),
  c(0.5,0),
  c(1,0),
  c(0.5,0.5),
  c(1,0.5),
  c(1,1),
  # transient graph nodes
  c(0.25,0.25),
  c(0.75,0.5),
  c(0.75,0.75),
  c(0.75,0.5),
  c(0.75,0.75),
  c(0.75,0.75)
)),coords=c(1,2))

# test_that("C++ persistent_graph",{
  pg_re<- array(seq(6*3*2),dim=c(6,3,2))
  pg_graph<- construct_dag(
    points[1:6,],
    new("staRVe_settings",n_neighbours=2)
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "persistent_graph",
      pg_re = pg_re,
      pg_edges = edges(staRVe:::idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag)
    ),
    para = list(
      dummy = 0
    ),
    DLL = "staRVe_model"
  )
  report<- obj$report()

  expect_equal(report$one_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,1,drop=FALSE])
  expect_equal(report$small_s_re, pg_re[c(1,3,5),,,drop=FALSE])
  expect_equal(report$small_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),,])
  expect_equal(report$small_g_di, distances(pg_graph$dag)[[2]])
  expect_equal(report$one_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,1,drop=FALSE])
  expect_equal(report$small_t_re, pg_re[,2:3,,drop=FALSE])
  expect_equal(report$small_v_re, pg_re[,,2,drop=FALSE])
# })

library(staRVe)
library(testthat)

# test_that("C++ transient_graph",{
  tg_re<- array(seq(6*2),dim=c(6,2))
  tg_graph<- construct_obs_dag(
    points[7:12,],
    pg_graph$locations,
    time = c(0,0,0,2,3,3),
    new("staRVe_settings",n_neighbours=2)
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "transient_graph",
      pg_re = pg_re,
      pg_edges = edges(staRVe:::idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag),
      tg_re = tg_re,
      tg_edges = edges(staRVe:::idxR_to_C(tg_graph)),
      tg_dists = distances(tg_graph),
      t = c(0,0,0,2,3,3)
    ),
    para = list(
      dummy = 0
    ),
    DLL = "staRVe_model"
  )
  report<- obj$report()

  expect_equal(report$small_t_re,tg_re[1:3,,drop=FALSE])
  expect_equal(report$small_v_re,tg_re[,2,drop=FALSE])
  expect_equal(report$small_tg_re,rbind(tg_re[edges(tg_graph)[[3]]$to,,drop=FALSE],
                                        pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,,drop=FALSE]))
  expect_equal(report$small_tg_di,distances(tg_graph)[[3]])

# })






library(staRVe)
library(testthat)
# test_that("C++ conditional normal",{
  x<- 1:4
  mu<- 10:14
  sigma<- 0.6^abs(outer(1:4,1:4,`-`))
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "kriging",
      x = x,
      mu = mu,
      sigma = sigma
    ),
    para = list(
      dummy = 0
    ),
    DLL = "staRVe_model"
  )

  report<- obj$report()
  expect_equal(report$conditional_mean,condMVNorm::condMVN(mu,sigma,1:2,3:4,x[3:4]))
  expect_equal(-1*report$loglikelihood,condMVNorm::dcmvnorm(x[1:2],mu,sigma,1:2,3:4,x[3:4],log=TRUE))
  expect_equal(length(report$sim),2)
# })
