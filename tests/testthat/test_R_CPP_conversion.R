test_that("Covariance to code",{
  # Check the expected values of test with covariance.hpp
  expect_equal(.covariance_to_code("exponential"),0)
  expect_equal(.covariance_to_code("gaussian"),1)
  expect_equal(.covariance_to_code("matern"),2)
  expect_equal(.covariance_to_code("matern32"),3)
  expect_error(.covariance_to_code("UNUSED"),"Supplied covariance")
})

test_that("Distribution to code",{
  # Check the expected values of test with family.hpp
  expect_equal(.distribution_to_code("gaussian"),0)
  expect_equal(.distribution_to_code("poisson"),1)
  expect_equal(.distribution_to_code("negative binomial"),2)
  expect_equal(.distribution_to_code("bernoulli"),3)
  expect_equal(.distribution_to_code("gamma"),4)
  expect_equal(.distribution_to_code("lognormal"),5)
  expect_equal(.distribution_to_code("binomial"),6)
  expect_equal(.distribution_to_code("atLeastOneBinomial"),7)
  expect_equal(.distribution_to_code("compois"),8)
  expect_equal(.distribution_to_code("tweedie"),9)
  expect_error(.distribution_to_code("UNUSED"),"Supplied distribution")
})

test_that("Link to code",{
  # Check the expected values of test with family.hpp
  expect_equal(.link_to_code("identity"),0)
  expect_equal(.link_to_code("log"),1)
  expect_equal(.link_to_code("logit"),2)
  expect_error(.link_to_code("UNUSED"),"Supplied link")
})

test_that("Logical to map",{
  expect_equal(.logical_to_map(rep(FALSE,4)),as.factor(1:4))
  expect_equal(.logical_to_map(c(TRUE,TRUE,FALSE,FALSE,TRUE)),as.factor(c(NA,NA,3,4,NA)))
  expect_error(.logical_to_map(c(5)),"Only logical")

  expect_equal(.logical_to_map(matrix(FALSE,nrow=2,ncol=4)),as.factor(1:8))
  expect_equal(.logical_to_map(cbind(c(FALSE,FALSE),c(NA,NA))),as.factor(c(1,2,NA,NA)))
  expect_equal(.logical_to_map(cbind(c(TRUE,FALSE),c(FALSE,TRUE))),as.factor(c(NA,2,3,NA)))
})

test_that("TMB in (R to C++)",{
  p<- cbind(rep(seq(0,1,by=0.5),3),rep(seq(0,1,by=0.5),each=3))
  p<- apply(p,1,sf::st_point,simplify=FALSE)
  p<- sf::st_sfc(p)


  ### Single year
  df<- sf::st_sf(
    y = 1:9,
    t = 0,
    geom = p
  )
  sm<- prepare_staRVe_model(
    y ~ time(t),
    df,
    n_neighbours = 3
  )
  TMB_in<- TMB_in(sm)

  expect_equal(TMB_in$data$model,"staRVe_model")
  expect_equal(TMB_in$data$distribution_code,.distribution_to_code("gaussian"))
  expect_equal(TMB_in$data$link_code,.link_to_code("identity"))
  expect_equal(TMB_in$data$resp_w_time,integer(0))
  expect_equal(TMB_in$data$mean_design,matrix(TRUE,nrow=9,ncol=0,dimnames=list(NULL,NULL)))
  expect_equal(TMB_in$data$sample_size,matrix(1,nrow=9,ncol=1,dimnames=list(NULL,list("V1"))))
  expect_equal(TMB_in$data$covar_code,.covariance_to_code("exponential"))
  expect_equal(TMB_in$data$pred_w_time,integer(0))

  expect_equal(dim(TMB_in$para$working_response_pars),c(1,1))
  expect_equal(dim(TMB_in$para$mean_pars),c(0,1))
  expect_equal(length(TMB_in$para$resp_w),0)
  expect_equal(dim(TMB_in$para$working_space_pars),c(3,1))
  expect_equal(dim(TMB_in$para$time_effects),c(1,1))
  expect_equal(dim(TMB_in$para$working_time_pars),c(3,1))
  expect_equal(dim(TMB_in$para$proc_w),c(9,1,1))


  ### Multiple years with some non-node observations
  df<- sf::st_sf(
    y = c(1:27,28:30),
    x1 = c(1,2),
    x2 = c(-1,-2),
    ss = c(2,3,4),
    t = c(rep(2000:2002,each=9),2001,2002,2002),
    geom = c(rep(p,3),sf::st_sfc(p[[1]]+c(0.3,0.3),p[[1]]+c(0.3,0.3),p[[1]]+c(0.7,0.7)))
  )
  sm<- prepare_staRVe_model(
    y ~ x1+x2+I(x1*x2)+time(t)+sample.size(ss)+space("gaussian"),
    df,
    nodes = df[1:9,],
    n_neighbours = 3,
    distribution = "compois"
  )
  TMB_in<- TMB_in(sm)

  expect_equal(TMB_in$data$distribution_code,.distribution_to_code("compois"))
  expect_equal(TMB_in$data$link_code,.link_to_code("log"))
  expect_equal(TMB_in$data$resp_w_time,c(1,2,2))
  expect_equal(TMB_in$data$mean_design,matrix(with(dat(sm),c(x1,x2,x1*x2)),nrow=30,ncol=3,dimnames=list(NULL,c("x1","x2","I(x1 * x2)"))))
  expect_equal(TMB_in$data$sample_size,matrix(dat(sm)$ss,nrow=30,ncol=1,dimnames=list(NULL,list("ss"))))
  expect_equal(TMB_in$data$covar_code,.covariance_to_code("gaussian"))
  expect_equal(TMB_in$data$pred_w_time,integer(0))

  expect_equal(dim(TMB_in$para$working_response_pars),c(1,1))
  expect_equal(dim(TMB_in$para$mean_pars),c(3,1))
  expect_equal(length(TMB_in$para$resp_w),3)
  expect_equal(dim(TMB_in$para$working_space_pars),c(3,1))
  expect_equal(dim(TMB_in$para$time_effects),c(3,1))
  expect_equal(dim(TMB_in$para$working_time_pars),c(3,1))
  expect_equal(dim(TMB_in$para$proc_w),c(9,3,1))
})

# -- update_staRVe_model (from staRVe_model.R)
