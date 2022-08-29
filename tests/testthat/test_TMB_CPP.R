points<- sf::st_as_sf(
  as.data.frame(
    rbind(
      # persistent graph nodes
      c(0, 0),
      c(0.5, 0),
      c(1, 0),
      c(0.5, 0.5),
      c(1, 0.5),
      c(1, 1),
      # transient graph nodes
      c(0.25, 0.25),
      c(0.75, 0.5),
      c(0.75, 0.75),
      c(0.75, 0.5),
      c(0.75, 0.75),
      c(0.75, 0.75)
    )
  ),
  coords=c(1, 2)
)



test_that("C++ covariance", {
  pars<- c(1, 2)
  covar_code<- 0
  d<- as.matrix(
    dist(1:4, diag = TRUE, upper = TRUE)
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "covariance",
      pars = pars,
      covar_code = covar_code,
      d = d
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  cond_sigma<- function(S, nc = 0) {
    if( nc == 0 ) {
      return(s)
    } else {}
    n1<- nrow(S) - nc
    n2<- nrow(S)
    S11<- S[seq(n1), seq(n1)]
    S12<- S[seq(n1), seq(n1 + 1, n2)]
    S22<- S[seq(n1 + 1, n2), seq(n1 + 1, n2)]
    return(S11 - S12 %*% solve(S22) %*% t(S12))
  }

  expect_equal(
    report$sd_1,
    1.0
  )
  expect_equal(
    report$sd_2,
    2.0
  )
})



test_that("C++ conditional normal", {
  x<- 1:4
  mu<- 10:13
  sigma<- 0.6^abs(outer(1:4, 1:4, `-`))
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "conditional_normal",
      x = x,
      mu = mu,
      sigma = sigma
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  expect_equal(
    report$conditional_mean,
    condMVNorm::condMVN(mu, sigma, 1:2, 3:4, x[3:4])$condMean
  )
  expect_equal(
    report$conditional_sigma,
    condMVNorm::condMVN(mu, sigma, 1:2, 3:4, x[3:4])$condVar
  )
  expect_equal(
    report$loglikelihood,
    condMVNorm::dcmvnorm(x[1:2], mu, sigma, 1:2, 3:4, x[3:4], log = TRUE)
  )
  expect_equal(
    length(report$sim),
    2
  )

  expect_equal(
    report$marginal_mean,
    mu
  )
  expect_equal(
    report$marginal_sigma,
    sigma
  )
})



test_that("C++ time series",{
  ts_re<- array(seq(20 * 3), dim = c(20, 3))
  ts_pars<- cbind(
    c(0, 0.6, 2),
    c(4, -0.2, 0.5),
    c(-4, -0.9, 0.1)
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "time_series",
      ts_re = ts_re,
      ts_pars = ts_pars
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  expect_equal(
    report$small_t_re,
    ts_re[3:5, , drop = FALSE]
  )
  expect_equal(
    report$small_v_re,
    ts_re[, 1, drop = FALSE]
  )
  expect_equal(
    report$loglikelihood,
    do.call(
      sum,
      lapply(1:3, function(v) {
        mvtnorm::dmvnorm(
          ts_re[, v],
          rep(ts_pars[1, v], nrow(ts_re)),
          (1 / (1 - ts_pars[2, v]^2)) *
            ts_pars[3, v]^2 *
            ts_pars[2, v]^abs(
              outer(
                seq(nrow(ts_re)),
                seq(nrow(ts_re)),
                `-`
              )
            ),
          log = TRUE
        )
      })
    )
  )
  expect_equal(
    dim(report$sim),
    dim(ts_re)
  )
})



test_that("C++ persistent_graph", {
  pg_re<- array(seq(6 * 3 * 2), dim = c(6, 3, 2))
  pg_graph<- construct_persistent_graph(
    points[1:6, ],
    new("settings", n_neighbours = 2)
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "persistent_graph",
      pg_re = pg_re,
      pg_edges = edges(convert_idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag)
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  expect_equal(
    report$small_s_re,
    pg_re[c(1, 3, 5), , , drop = FALSE]
  )
  expect_equal(
    report$small_s_mean,
    pg_re[c(1, 3, 5), , , drop = FALSE]
  )

  expect_equal(
    report$small_g_re,
    pg_re[do.call(c, edges(pg_graph$dag)[[2]]), , ]
  )
  expect_equal(
    report$small_g_mean,
    pg_re[do.call(c, edges(pg_graph$dag)[[2]]), , ]
  )
  expect_equal(
    report$small_g_di,
    distances(pg_graph$dag)[[2]]
  )

  expect_equal(
    report$one_g_re,
    pg_re[do.call(c, edges(pg_graph$dag)[[2]]), 1, 1, drop = FALSE]
  )
  expect_equal(
    report$one_g_mean,
    pg_re[do.call(c, edges(pg_graph$dag)[[2]]), 1, 1, drop = FALSE]
  )

  expect_equal(
    report$small_t_re,
    pg_re[, 2:3, , drop = FALSE]
  )
  expect_equal(
    report$small_t_mean,
    pg_re[, 2:3, , drop = FALSE]
  )

  expect_equal(
    report$small_v_re,
    pg_re[, , 2, drop = FALSE]
  )
  expect_equal(
    report$small_v_mean,
    pg_re[, , 2, drop = FALSE]
  )


  pg_re[edges(pg_graph$dag)[[1]]$to, 1, 1]<- c(-0.5, -1.0)
  expect_equal(
    report$overwrite_re,
    pg_re
  )
  expect_equal(
    report$after_overwrite_re,
    pg_re
  )
  expect_equal(
    report$overwrite_re1,
    pg_re[edges(pg_graph$dag)[[1]]$to, 1, 1]
  )

  pg_mean<- pg_re
  pg_mean[edges(pg_graph$dag)[[1]]$to, 1, 1]<- c(-20, -21)
  expect_equal(
    report$overwrite_mean,
    pg_mean
  )
  expect_equal(
    report$after_overwrite_mean,
    pg_mean
  )
  expect_equal(
    report$overwrite_mean1,
    pg_mean[edges(pg_graph$dag)[[1]]$to, 1, 1]
  )
})



test_that("C++ transient_graph", {
  pg_re<- array(seq(6 * 3 * 2), dim = c(6, 3, 2))
  pg_graph<- construct_persistent_graph(
    points[1:6, ],
    new("settings", n_neighbours = 2)
  )

  tg_re<- array(seq(6 * 2), dim = c(6, 2))
  tg_graph<- construct_transient_graph(
    points[7:12, ],
    pg_graph$locations,
    time = c(0, 0, 0, 2, 3, 3),
    new("settings", n_neighbours = 2)
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "transient_graph",
      pg_re = pg_re,
      pg_edges = edges(convert_idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag),
      tg_re = tg_re,
      tg_edges = edges(convert_idxR_to_C(tg_graph)),
      tg_dists = distances(tg_graph),
      t = c(0, 0, 0, 2, 3, 3)
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  expect_equal(
    report$full_re,
    tg_re
  )
  expect_equal(
    report$full_mean,
    tg_re
  )

  expect_equal(
    report$small_tg_re,
    rbind(
      tg_re[edges(tg_graph)[[3]]$to, , drop = FALSE],
      pg_re[, 1, , drop = TRUE][edges(tg_graph)[[3]]$from, , drop = FALSE]
    )
  )
  expect_equal(
    report$small_tg_mean,
    rbind(
      tg_re[edges(tg_graph)[[3]]$to, , drop = FALSE],
      pg_re[, 1, , drop = TRUE][edges(tg_graph)[[3]]$from, , drop = FALSE]
    )
  )
  expect_equal(
    report$small_tg_di,
    unname(distances(tg_graph)[[3]])
  )

  expect_equal(
    report$ssmall_tg_re,
    rbind(
      tg_re[edges(tg_graph)[[3]]$to, 1, drop = FALSE],
      pg_re[, 1, , drop = TRUE][edges(tg_graph)[[3]]$from, 1, drop = FALSE]
    )
  )
  expect_equal(
    report$ssmall_tg_mean,
    rbind(
      tg_re[edges(tg_graph)[[3]]$to, 1, drop = FALSE],
      pg_re[, 1, , drop = TRUE][edges(tg_graph)[[3]]$from, 1, drop = FALSE]
    )
  )
  expect_equal(
    report$ssmall_tg_di,
    unname(distances(tg_graph)[[3]])
  )

  expect_equal(
    report$small_t_re,
    tg_re[1:3, , drop = FALSE]
  )
  expect_equal(
    report$small_t_mean,
    tg_re[1:3, , drop = FALSE]
  )

  expect_equal(
    report$small_v_re,
    tg_re[, 2, drop = FALSE]
  )
  expect_equal(
    report$small_v_mean,
    tg_re[, 2, drop = FALSE]
  )

  tg_re[edges(tg_graph)[[1]]$to, 1]<- c(-0.5)
  expect_equal(
    report$overwrite_re,
    tg_re
  )
  expect_equal(
    report$after_overwrite_re,
    tg_re
  )

  tg_re[edges(tg_graph)[[1]]$to, 1]<- c(-20.0)
  expect_equal(
    report$overwrite_mean,
    tg_re
  )
  expect_equal(
    report$after_overwrite_mean,
    tg_re
  )
  expect_equal(
    report$overwrite_mean1,
    c(
      tg_re[edges(tg_graph)[[1]]$to, 1],
      pg_re[edges(tg_graph)[[1]]$from, 1, 1]
    )
  )
})





test_that("C++ nngp", {
  nt<- 10
  ts_re<- array(seq(nt * 2), dim = c(nt, 2))
  ts_pars<- cbind(
    c(0, 0.6, 2),
    c(4, -0.2, 0.5)
  )

  pg_re<- array(seq(6 * nt * 2), dim = c(6, nt, 2))
  pg_graph<- construct_persistent_graph(
    points[1:6, ],
    new("settings", n_neighbours = 2)
  )

  tg_re<- array(seq(nt * 2), dim = c(nt, 2))
  tg_time<- seq(nt) - 1
  tg_time[2:3]<- 0
  tg_graph<- construct_transient_graph(
    points[rep(8, nt), ],
    pg_graph$locations,
    time = tg_time,
    new("settings", n_neighbours = 2)
  )

  cv_pars<- cbind(
    c(1, 0.5),
    c(2, 0.3)
  )
  cv_code<- c(0, 0)

  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "nngp",
      ts_re = ts_re,
      ts_pars = ts_pars,
      pg_re = pg_re,
      pg_edges = edges(convert_idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag),
      tg_re = tg_re,
      tg_edges = edges(convert_idxR_to_C(tg_graph)),
      tg_dists = distances(tg_graph),
      t = tg_time,
      cv_pars = cv_pars,
      cv_code = cv_code
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  expect_equal(
    report$bnngp_pg_re,
    pg_re
  )
  expect_equal(
    report$bnngp_pg_mean,
    pg_re
  )

  # loglikelihood computed here; doesn't make sense to check it

  expect_equal(
    report$one_pg_re,
    pg_re[2, 1, 1]
  )
  expect_equal(
    report$one_tg_re,
    tg_re[3, 2]
  )

  expect_equal(
    report$nngp_pg_re,
    pg_re
  )
  expect_false(
    isTRUE(
      all.equal(
        report$nngp_pg_mean,
        pg_re
      )
    )
  )


  tre<- report$ts_sim
  pg_re<- report$sim_nngp_pg_re

  R_mean<- array(0, dim = dim(pg_re))
  for( v in 1:2 ) {
    R_mean[, 1, v]<- tre[1, v]
    for( t in 2:nt ) {
      R_mean[, t, v]<- tre[t, v] + ts_pars[2, v] *
        (pg_re[, t - 1, v] - tre[t - 1, v])
    }
  }
  expect_equal(
    report$sim_nngp_pg_mean,
    R_mean
  )

  # loglikilihood for simulations computed here; doesn't make sense to check it
})





test_that("C++ inv_link_function", {
  x<- seq(-5, 5, 0.5)
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "inv_link_function",
      x = x
    ),
    para = list(dummy = 0),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  expect_equal(
    report$ans[, 1],
    x
  )
  expect_equal(
    report$ans[, 2],
    exp(x)
  )
  expect_equal(
    report$ans[, 3],
    plogis(x)
  )
})



test_that("C++ distribution", {
  xsize<- 20
  x<- cbind(
    seq(-5, 5, length.out = xsize), # Normal
    seq(xsize) - 1, # Poisson
    seq(xsize) - 1, # Neg. Binom.
    c(0, 1), # Bernoulli
    seq(0, 5, length.out = xsize), # Gamma
    seq(0, 5, length.out = xsize) + 0.1, # Log-Normal
    seq(xsize) - 1, # Binomial
    c(0, 1), # atLeastOneBinomial
    seq(xsize) - 1, # Compois
    seq(0, 5, length.out = xsize), # Tweedie
    seq(-5, 5, length.out = xsize) # Student's t
  )
  mean<- c(
    0, # Normal
    10, # Poisson
    10, # Neg. Binom.
    0.5, # Bernoulli
    3, # Gamma
    0, # Log-normal
    0.5, # Binomial
    0.5, # atLeastOneBinomial
    10, # Compois
    3, # tweedie
    0 # Student's t
  )
  pars<- cbind(
    1, # Normal (sd)
    NA, # Poisson,
    2, # Neg. Binomial (overdispersion)
    NA, # Bernoulli
    1, # Gamma (sd)
    1, # Log-normal (sd)
    NA, # Binomial
    NA, # atLeastOneBinomial
    1, # Compois (sd)
    c(0.5, 1.5), # tweedie (dispersion,power)
    5 # Student's t (df)
  )
  size<- cbind(
    NA, # Normal
    NA, # Poisson
    NA, # Neg. Binom.
    NA, # Bernoulli
    NA, # Gamma
    NA, # Log-Normal
    rep(xsize, xsize), # Binomial
    4, # atLeastOneBinomial
    NA, # compois
    1, # tweedie
    NA # Student's t
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "distribution",
      x = x,
      mean = mean,
      pars = pars,
      size = size
    ),
    para = list(dummy = 0),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  # Normal
  expect_equal(
    report$ans[, 1],
    dnorm(x[, 1], mean[[1]], pars[[1, 1]], log = TRUE)
  )

  # Poisson
  expect_equal(
    report$ans[, 2],
    dpois(x[, 2], mean[[2]], log = TRUE)
  )

  # Neg. Binomial
  # var = par*mean = mean + mu^2/size
  # 1/size = (par-1)*mean / mean^2
  # size = mean/(par-1)
  expect_equal(
    report$ans[, 3],
    dnbinom(
      x[, 3],
      mu = mean[[3]],
      size = mean[[3]] / (pars[[1, 3]] - 1),
      log = TRUE
    )
  )

  # Bernoulli
  expect_equal(
    report$ans[,4 ],
    dbinom(x[, 4], size = 1, prob = mean[[4]], log = TRUE)
  )

  # Gamma
  expect_equal(
    report$ans[, 5],
    dgamma(
      x[, 5],
      shape = (mean[[5]] / pars[[1, 5]])^2,
      scale = pars[[1, 5]]^2 / mean[[5]],
      log = TRUE
    )
  )

  # Log-normal
  expect_equal(
    report$ans[, 6],
    dlnorm(x[, 6], meanlog = mean[[6]], sdlog = pars[[1, 6]], log = TRUE)
  )

  # Binomial
  expect_equal(
    report$ans[, 7],
    dbinom(x[, 7], size = size[, 7], prob = mean[[7]], log = TRUE)
  )

  # atLeastOneBinomial
  expect_equal(
    report$ans[, 8],
    ifelse(x[, 8] == 0,
      dbinom(0, size = size[, 8], prob = mean[[8]], log = TRUE),
      log(1 - dbinom(0, size = size[,8], prob = mean[[8]], log = FALSE))
    )
  )

  # Compois
    # No readily available R implementation

  # Tweedie
    # No readily available R implementation

  # Student's t
  expect_equal(
    report$ans[, 11],
    dt(x[, 11] - mean[[11]], df = pars[[1, 11]], log = TRUE)
  )
})



test_that("C++ family", {
  xsize<- 20
  x<- cbind(
    seq(-5, 5, length.out = xsize), # Normal
    seq(xsize) - 1, # Poisson
    seq(xsize) - 1, # Neg. Binom.
    c(0, 1), # Bernoulli
    seq(0, 5, length.out = xsize), # Gamma
    seq(0, 5, length.out = xsize) + 0.1, # Log-Normal
    seq(xsize) - 1, # Binomial
    c(0, 1), # atLeastOneBinomial
    seq(xsize) - 1, # Compois
    seq(0, 5, length.out = xsize), # Tweedie
    seq(-5, 5, length.out = xsize) # Student's t
  )
  mean<- c(
    0, # Normal
    log(10), # Poisson
    log(10), # Neg. Binom.
    qlogis(0.5), # Bernoulli
    log(3), # Gamma-
    0, # Log-normal
    qlogis(0.5), # Binomial
    qlogis(0.5), # atLeastOneBinomial
    log(10), # Compois
    log(3), # tweedie
    0 # Student's t
  )
  pars<- cbind(
    1, # Normal (sd)
    NA, # Poisson,
    2, # Neg. Binomial (overdispersion)
    NA, # Bernoulli
    1, # Gamma (sd)
    1, # Log-normal (sd)
    NA, # Binomial
    NA, # atLeastOneBinomial
    1, # Compois (sd)
    c(0.5, 1.5), # tweedie (dispersion,power)
    5 # Student's t (df)
  )
  size<- cbind(
    NA, # Normal
    NA, # Poisson
    NA, # Neg. Binom.
    NA, # Bernoulli
    NA, # Gamma
    NA, # Log-Normal
    rep(xsize, xsize), # Binomial
    4, # atLeastOneBinomial
    NA, # compois
    1, # tweedie
    NA # Student's t
  )
  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "family",
      x = x,
      mean = mean,
      pars = pars,
      size = size
    ),
    para = list(dummy = 0),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  # Normal
  expect_equal(
    report$ans[, 1],
    dnorm(x[, 1], mean[[1]], pars[[1, 1]], log = TRUE)
  )

  # Poisson
  expect_equal(
    report$ans[, 2],
    dpois(x[, 2], exp(mean[[2]]), log = TRUE)
  )

  # Neg. Binomial
  # var = par*mean = mean + mu^2/size
  # 1/size = (par-1)*mean / mean^2
  # size = mean/(par-1)
  expect_equal(
    report$ans[,3],
    dnbinom(
      x[, 3],
      mu = exp(mean[[3]]),
      size = exp(mean[[3]]) / (pars[[1, 3]] - 1),
      log = TRUE
    )
  )

  # Bernoulli
  expect_equal(
    report$ans[, 4],
    dbinom(x[, 4], size = 1, prob = plogis(mean[[4]]), log = TRUE)
  )

  # Gamma
  expect_equal(
    report$ans[, 5],
    dgamma(
      x[, 5],
      shape = (exp(mean[[5]]) / pars[[1, 5]])^2,
      scale = pars[[1, 5]]^2 / exp(mean[[5]]),
      log = TRUE
    )
  )

  # Log-normal
  expect_equal(
    report$ans[, 6],
    dlnorm(x[, 6], meanlog = mean[[6]], sdlog = pars[[1, 6]], log = TRUE)
  )

  # Binomial
  expect_equal(
    report$ans[, 7],
    dbinom(x[, 7], size = size[, 7], prob = plogis(mean[[7]]), log = TRUE)
  )

  # atLeastOneBinomial
  expect_equal(
    report$ans[, 8],
    ifelse(x[, 8] == 0,
      dbinom(0, size = size[, 8], prob = plogis(mean[[8]]), log = TRUE),
      log(1 - dbinom(0, size = size[, 8], prob = plogis(mean[[8]]), log = FALSE))
    )
  )

  # Compois
    # No readily available R implementation

  # Tweedie
    # No readily available R implementation

  # Student's t
  expect_equal(
    report$ans[, 11],
    dt(x[, 11] - mean[[11]], df = pars[[1, 11]], log = TRUE)
  )
})



test_that("C++ observations", {
  nt<- 10
  ts_re<- array(seq(nt * 2), dim = c(nt, 2))
  ts_pars<- cbind(
    c(4, 0.6, 2),
    c(-5, -0.2, 0.5)
  )

  pg_re<- array(seq(6 * nt * 2), dim = c(6, nt, 2))
  pg_graph<- construct_persistent_graph(
    points[1:6, ],
    new("settings", n_neighbours = 2)
  )

  tg_re<- array(seq(nt * 2), dim = c(nt, 2))
  tg_time<- seq(nt) - 1
  tg_time[2:3]<- 0
  tg_graph<- construct_transient_graph(
    points[rep(8, nt), ],
    pg_graph$locations,
    time = tg_time,
    new("settings", n_neighbours = 2)
  )

  cv_pars<- cbind(
    c(1, 0.5),
    c(2, 0.3)
  )
  cv_code<- c(0, 0)

  nobs<- 30
  obs<- matrix(seq(nobs * 2), ncol = 2)
  obs[c(2, 6), 1]<- NA
  obs[c(3, 7), 2]<- NA

  set.seed(1234)
  idx<- matrix(0, nrow = nobs, ncol = 2)
  idx[, 2]<- sort(sample(nt, nobs, replace = TRUE))
  obspert<- tabulate(idx[, 2])
  locspert<- 6 + tabulate(tg_time + 1)
  idx[, 1]<- do.call(
    c,
    Map(
      sample,
      x = locspert,
      size = obspert,
      replace = TRUE
    )
  )
  idx[1, ]<- c(locspert[[1]], 1) # Ensure a transient graph node

  sample_size<- obs
  mean_design<- matrix(rnorm(nobs * 2), ncol = 2)
  beta<- matrix(seq(4), ncol = 2)
  family_codes<- cbind(
    c(0, 0), # identity link, Normal distribution
    c(1, 1) # log link, Poisson distribution
  )

  family_pars<- cbind(
    c(1), # Normal std. dev.
    c(NA)
  )

  obj<- TMB::MakeADFun(
    data = list(
      model = "testing",
      test = "observations",
      ts_re = ts_re,
      ts_pars = ts_pars,
      pg_re = pg_re,
      pg_edges = edges(convert_idxR_to_C(pg_graph$dag)),
      pg_dists = distances(pg_graph$dag),
      tg_re = tg_re,
      tg_edges = edges(convert_idxR_to_C(tg_graph)),
      tg_dists = distances(tg_graph),
      t = tg_time,
      cv_pars = cv_pars,
      cv_code = cv_code,
      obs = obs,
      idx = idx - 1,
      sample_size = sample_size,
      mean_design = mean_design,
      beta = beta,
      family_codes = family_codes,
      family_pars = family_pars
    ),
    para = list(
      dummy = 0
    ),
    DLL = "starve_TMB"
  )
  report<- obj$report()

  # Compute means before simulation
  mm<- obs
  fixed<- mean_design %*% beta
  for( i in seq(nrow(mm)) ) {
    for( v in seq(ncol(mm)) ) {
      s<- idx[i, 1]
      t<- idx[i, 2]
      mm[[i, v]]<- fixed[i, v] + c(
        pg_re[, t, v],
        tg_re[(tg_time + 1) == t, v]
      )[[s]]
    }
  }
  ll<- sum(
    c(
      dnorm(obs[, 1], mm[, 1], family_pars[, 1], log = TRUE),
      dpois(obs[, 2], exp(mm[, 2]), log = TRUE)
    ),
    na.rm = TRUE
  )
  expect_equal(
    report$mm,
    mm
  )
  expect_equal(
    report$ll,
    ll
  )

  # Compute means after simulation
  new_mm<- obs
  fixed<- mean_design %*% beta
  for( i in seq(nrow(mm)) ) {
    for( v in seq(ncol(mm)) ) {
      s<- idx[i, 1]
      t<- idx[i, 2]
      new_mm[[i, v]]<- fixed[i, v] + c(
        report$new_pg[, t, v],
        report$new_tg[(tg_time+1) == t, v]
      )[[s]]
    }
  }
  new_ll<- sum(
    c(
      dnorm(report$new_obs[, 1], new_mm[, 1], family_pars[, 1], log = TRUE),
      dpois(report$new_obs[, 2], exp(new_mm[, 2]), log = TRUE)
    ),
    na.rm = TRUE
  )
  expect_equal(
    report$new_mm,
    new_mm
  )
  expect_equal(
    report$new_ll,
    new_ll
  )
})
