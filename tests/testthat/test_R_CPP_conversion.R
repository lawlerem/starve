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

# -- TMB_in (cross-check the different parts) (from staRVe_model.R)
# -- update_staRVe_model (from staRVe_model.R)
