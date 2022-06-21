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



# test_that("C++ covariance",{
#   pars<- c(1,2)
#   covar_code<- 0
#   d<- as.matrix(dist(1:4,diag=TRUE,upper=TRUE))
#   obj<- TMB::MakeADFun(
#     data = list(
#       model = "testing",
#       test = "covariance",
#       pars = pars,
#       covar_code = covar_code,
#       d = d
#     ),
#     para = list(
#       dummy = 0
#     ),
#     DLL = "staRVe_model"
#   )
#   obj$fn()
#   # Just want to check that obj can be created
# #}
#
#
# # test_that("C++ conditional normal",{
#   x<- 1:4
#   mu<- 10:13
#   sigma<- 0.6^abs(outer(1:4,1:4,`-`))
#   obj<- TMB::MakeADFun(
#     data = list(
#       model = "testing",
#       test = "conditional_normal",
#       x = x,
#       mu = mu,
#       sigma = sigma
#     ),
#     para = list(
#       dummy = 0
#     ),
#     DLL = "staRVe_model"
#   )
#   report<- obj$report()
#
#   expect_equal(report$conditional_mean,condMVNorm::condMVN(mu,sigma,1:2,3:4,x[3:4])$condMean)
#   expect_equal(report$conditional_sigma,condMVNorm::condMVN(mu,sigma,1:2,3:4,x[3:4])$condVar)
#   expect_equal(report$loglikelihood,condMVNorm::dcmvnorm(x[1:2],mu,sigma,1:2,3:4,x[3:4],log=TRUE))
#   expect_equal(length(report$sim),2)
#
#   expect_equal(report$marginal_mean,mu)
#   expect_equal(report$marginal_sigma,sigma)
# # })
#
#
#
#
# # test_that("C++ time series",{
#   ts_re<- array(seq(20*3),dim=c(20,3))
#   ts_pars<- cbind(c(0,0.6,2),
#                   c(4,-0.2,0.5),
#                   c(-4,-0.9,0.1))
#   obj<- TMB::MakeADFun(
#     data = list(
#       model = "testing",
#       test = "time_series",
#       ts_re = ts_re,
#       ts_pars = ts_pars
#     ),
#     para = list(
#       dummy = 0
#     ),
#     DLL = "staRVe_model"
#   )
#   report<- obj$report()
#
#   expect_equal(report$small_t_re,ts_re[3:5,,drop=FALSE])
#   expect_equal(report$small_v_re,ts_re[,1,drop=FALSE])
#   expect_equal(report$loglikelihood,
#     do.call(sum,lapply(1:3,function(v) {
#       mvtnorm::dmvnorm(
#         ts_re[,v],
#         rep(ts_pars[1,v],nrow(ts_re)),
#         ts_pars[3,v]^2*ts_pars[2,v]^abs(outer(seq(nrow(ts_re)),seq(nrow(ts_re)),`-`)),
#         log = TRUE
#       )
#     }))
#   )
#   expect_equal(dim(report$sim),dim(ts_re))
# #})
#
#
#
#
# # test_that("C++ persistent_graph",{
#   pg_re<- array(seq(6*3*2),dim=c(6,3,2))
#   pg_graph<- construct_dag(
#     points[1:6,],
#     new("staRVe_settings",n_neighbours=2)
#   )
#   obj<- TMB::MakeADFun(
#     data = list(
#       model = "testing",
#       test = "persistent_graph",
#       pg_re = pg_re,
#       pg_edges = edges(staRVe:::idxR_to_C(pg_graph$dag)),
#       pg_dists = distances(pg_graph$dag)
#     ),
#     para = list(
#       dummy = 0
#     ),
#     DLL = "staRVe_model"
#   )
#   report<- obj$report()
#
#   expect_equal(report$small_s_re, pg_re[c(1,3,5),,,drop=FALSE])
#   expect_equal(report$small_s_mean, pg_re[c(1,3,5),,,drop=FALSE])
#
#   expect_equal(report$small_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),,])
#   expect_equal(report$small_g_mean, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),,])
#   expect_equal(report$small_g_di, distances(pg_graph$dag)[[2]])
#
#   expect_equal(report$one_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,1,drop=FALSE])
#   expect_equal(report$one_g_mean, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,1,drop=FALSE])
#
#   expect_equal(report$small_t_re, pg_re[,2:3,,drop=FALSE])
#   expect_equal(report$small_t_mean, pg_re[,2:3,,drop=FALSE])
#
#   expect_equal(report$small_v_re, pg_re[,,2,drop=FALSE])
#   expect_equal(report$small_v_mean, pg_re[,,2,drop=FALSE])
#
#
#   pg_re[edges(pg_graph$dag)[[1]]$to,1,1]<- c(-0.5,-1.0)
#   expect_equal(report$overwrite_re, pg_re)
#   expect_equal(report$after_overwrite_re, pg_re)
#   expect_equal(report$overwrite_re1, pg_re[edges(pg_graph$dag)[[1]]$to,1,1])
#
#   pg_mean<- pg_re
#   pg_mean[edges(pg_graph$dag)[[1]]$to,1,1]<- c(-20,-21)
#   expect_equal(report$overwrite_mean, pg_mean)
#   expect_equal(report$after_overwrite_mean, pg_mean)
#   expect_equal(report$overwrite_mean1, pg_mean[edges(pg_graph$dag)[[1]]$to,1,1])
# # })
#
#
#
#
#
# test_that("C++ transient_graph",{
  pg_re<- array(seq(6*3*2),dim=c(6,3,2))
  pg_graph<- construct_dag(
    points[1:6,],
    new("staRVe_settings",n_neighbours=2)
  )

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

  expect_equal(report$full_re,tg_re)
  expect_equal(report$full_mean,tg_re)

  expect_equal(report$small_tg_re,rbind(tg_re[edges(tg_graph)[[3]]$to,,drop=FALSE],
                                        pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,,drop=FALSE]))
  expect_equal(report$small_tg_mean,rbind(tg_re[edges(tg_graph)[[3]]$to,,drop=FALSE],
                                          pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,,drop=FALSE]))
  expect_equal(report$small_tg_di,distances(tg_graph)[[3]])

  expect_equal(report$ssmall_tg_re,rbind(tg_re[edges(tg_graph)[[3]]$to,1,drop=FALSE],
                                         pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,1,drop=FALSE]))
  expect_equal(report$ssmall_tg_mean,rbind(tg_re[edges(tg_graph)[[3]]$to,1,drop=FALSE],
                                           pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,1,drop=FALSE]))
  expect_equal(report$ssmall_tg_di,distances(tg_graph)[[3]])

  expect_equal(report$small_t_re,tg_re[1:3,,drop=FALSE])
  expect_equal(report$small_t_mean,tg_re[1:3,,drop=FALSE])

  expect_equal(report$small_v_re,tg_re[,2,drop=FALSE])
  expect_equal(report$small_v_mean,tg_re[,2,drop=FALSE])

  tg_re[edges(tg_graph)[[1]]$to,1]<- c(-0.5)
  expect_equal(report$overwrite_re, tg_re)
  expect_equal(report$after_overwrite_re, tg_re)

  tg_re[edges(tg_graph)[[1]]$to,1]<- c(-20.0)
  expect_equal(report$overwrite_mean, tg_re)
  expect_equal(report$after_overwrite_mean, tg_re)
  expect_equal(report$overwrite_mean1, c(tg_re[edges(tg_graph)[[1]]$to,1],
                                         pg_re[edges(tg_graph)[[1]]$from,1,1]))
# })





# test_that("C++ nngp",{
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


  nt<- 10
  ts_re<- array(seq(nt*2),dim=c(nt,2))
  ts_pars<- cbind(c(0,0.6,2),
                  c(4,-0.2,0.5))

  pg_re<- array(seq(6*nt*2),dim=c(6,nt,2))
  pg_graph<- construct_dag(
    points[1:6,],
    new("staRVe_settings",n_neighbours=2)
  )

  tg_re<- array(seq(nt*2),dim=c(nt,2))
  tg_time<- seq(nt)-1
  tg_time[2:3]<- 0
  tg_graph<- construct_obs_dag(
    points[rep(8,nt),],
    pg_graph$locations,
    time = tg_time,
    new("staRVe_settings",n_neighbours=2)
  )

  cv_pars<- cbind(c(1,0.5),
                  c(2,0.3))
  cv_code<- c(0,0)

  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "nngp",
      ts_re = ts_re,
      ts_pars = ts_pars,
      pg_re = pg_re,
      pg_edges = edges(staRVe:::idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag),
      tg_re = tg_re,
      tg_edges = edges(staRVe:::idxR_to_C(tg_graph)),
      tg_dists = distances(tg_graph),
      t = tg_time,
      cv_pars = cv_pars,
      cv_code = cv_code
    ),
    para = list(
      dummy = 0
    ),
    DLL = "staRVe_model"
  )
  report<- obj$report()

  expect_equal(report$bnngp_pg_re, pg_re)
  expect_equal(report$bnngp_pg_mean, pg_re)

  expect_equal(report$bnngp_pg_re, pg_re)
  expect_false(isTRUE(all.equal(report$nngp_pg_mean, pg_re)))


  expect_equal(report$small_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,,drop=FALSE])
  expect_equal(report$small_g_mean, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,,drop=FALSE])
  expect_equal(report$small_g_di, distances(pg_graph$dag)[[2]])

  expect_equal(report$one_g_re, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,1,drop=FALSE])
  expect_equal(report$one_g_mean, pg_re[do.call(c,edges(pg_graph$dag)[[2]]),1,1,drop=FALSE])



  expect_equal(report$small_tg_re,rbind(tg_re[edges(tg_graph)[[3]]$to,,drop=FALSE],
                                        pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,,drop=FALSE]))
  expect_equal(report$small_tg_mean,rbind(tg_re[edges(tg_graph)[[3]]$to,,drop=FALSE],
                                          pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,,drop=FALSE]))
  expect_equal(report$small_tg_di,distances(tg_graph)[[3]])

  expect_equal(report$ssmall_tg_re,rbind(tg_re[edges(tg_graph)[[3]]$to,1,drop=FALSE],
                                         pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,1,drop=FALSE]))
  expect_equal(report$ssmall_tg_mean,rbind(tg_re[edges(tg_graph)[[3]]$to,1,drop=FALSE],
                                           pg_re[,1,,drop=TRUE][edges(tg_graph)[[3]]$from,1,drop=FALSE]))
  expect_equal(report$ssmall_tg_di,distances(tg_graph)[[3]])

  # par(mfrow=c(4,1))
  #   plot.ts(data.frame(report$ts_sim),plot.type="single",lty=c(1,2))
  #   plot.ts(data.frame(t(report$sim_nngp_pg_re[,,1])),plot.type="single",lty=1)
  #   plot.ts(data.frame(t(report$sim_nngp_pg_re[,,2])),plot.type="single",lty=2)
  #   plot.ts(data.frame(report$sim_nngp_tg_re),plot.type="single",lty=c(1,2))
# })
