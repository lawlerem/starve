bbox<- sf::st_polygon(list(rbind(c(0,0),c(0,1),c(1,1),c(1,0),c(0,0))))
nt<- 5
npert<- 5

# covariance_from_formula
test_that("Covariance function from formula",{
  expect_covariance_function_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(covariance_from_formula(formula)$covariance,expected)
    ))
  }
  expect_nu_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(covariance_from_formula(formula)$nu,.(expected))
    ))
  }

  ff<- c(
    y ~ 1, # 1
    y ~ space("exponential"), # 2
    y ~ space("matern32"), # 3
    y ~ space("gaussian"), # 4
    y ~ space("matern"), # 5
    y ~ space("matern",nu = 1), # 6
    y ~ space("exponential")+space("matern32"), # 7
    cbind(y,response) ~ 1, # 8
    cbind(y,response) ~ space("exponential"), # 9
    cbind(y,response) ~ space(c("exponential","matern32")), # 10
    cbind(y,response) ~ space(c("matern","matern"),c(0.5,NaN)) # 11
  )

  expect_covariance_function_equal(ff[[1]],"exponential")
  expect_covariance_function_equal(ff[[2]],"exponential")
  expect_covariance_function_equal(ff[[3]],"matern32")
  expect_covariance_function_equal(ff[[4]],"gaussian")
  expect_covariance_function_equal(ff[[5]],"matern")
  expect_nu_equal(ff[[5]],NaN)
  expect_covariance_function_equal(ff[[6]],"matern")
  eval(bquote(expect_error(covariance_from_formula(ff[[7]]),"Multiple")))
  expect_nu_equal(ff[[6]],1)
  expect_covariance_function_equal(ff[[8]],c("exponential","exponential"))
  expect_covariance_function_equal(ff[[9]],c("exponential","exponential"))
  expect_covariance_function_equal(ff[[10]],c("exponential","matern32"))
  expect_covariance_function_equal(ff[[11]],c("matern","matern"))
  expect_nu_equal(ff[[11]],c(0.5,NaN))
})

# mean_design_from_formula
test_that("Covariates from formula",{
  expect_val_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(mean_design_from_formula(formula,test_data,return_type),expected,ignore_attr=TRUE)
    ))
  }
  # expect_ncol is  ad hoc way to check I'm getting the variable expansions (factors, poly) I want
  expect_colnames_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(colnames(mean_design_from_formula(formula,test_data,return_type)),expected)
    ))
  }
  test_data<- sf::st_sf(
    y = rnorm(nt*npert),
    x1 = rnorm(nt*npert),
    x2 = rnorm(nt*npert),
    x3 = factor(rep(seq(nt),npert)),
    t = rep(seq(nt),each=npert),
    geom = sf::st_sample(bbox,nt*npert)
  )
  ff<- c(
    y ~ 1, # 1
    y ~ time(t), # 2
    y ~ x1, # 3
    y ~ x3, # 4
    y ~ x1 + time(t), # 5
    y ~ x1 + time(x1), # 6
    y ~ x1+x2, # 7
    y ~ x1*x2, # 8
    y ~ I(x1^2) + I(exp(x2)), # 9
    y ~ x1+x3, # 10
    y ~ poly(x1,2) # 11
  )

  # all.vars
  return_type<- "all.vars"
  expect_val_equal(ff[[1]],as.data.frame(matrix(0,nrow=nrow(test_data),ncol=0)))

  expect_val_equal(ff[[2]],as.data.frame(matrix(0,nrow=nrow(test_data),ncol=0)))

  expect_val_equal(ff[[3]],as.data.frame(test_data)[,"x1",drop=F])
  expect_colnames_equal(ff[[3]],"x1")

  expect_val_equal(ff[[4]],as.data.frame(test_data)[,"x3",drop=F])
  expect_colnames_equal(ff[[4]],"x3")

  expect_val_equal(ff[[5]],as.data.frame(test_data)[,"x1",drop=F])
  expect_colnames_equal(ff[[5]],"x1")

  expect_val_equal(ff[[6]],as.data.frame(test_data)[,"x1",drop=F])
  expect_colnames_equal(ff[[6]],"x1")

  expect_val_equal(ff[[7]],as.data.frame(test_data)[,c("x1","x2"),drop=F])
  expect_colnames_equal(ff[[7]],c("x1","x2"))

  expect_val_equal(ff[[8]],as.data.frame(test_data)[,c("x1","x2"),drop=F])
  expect_colnames_equal(ff[[8]],c("x1","x2"))

  expect_val_equal(ff[[9]],as.data.frame(test_data)[,c("x1","x2"),drop=F])
  expect_colnames_equal(ff[[9]],c("x1","x2"))

  expect_val_equal(ff[[10]],as.data.frame(test_data)[,c("x1","x3"),drop=F])
  expect_colnames_equal(ff[[10]],c("x1","x3"))

  expect_val_equal(ff[[11]],as.data.frame(test_data)[,"x1",drop=F])
  expect_colnames_equal(ff[[11]],"x1")




  # model.frame
  return_type<- "model.frame"
  expect_val_equal(ff[[1]],as.data.frame(matrix(0,nrow=nrow(test_data),ncol=0)))

  expect_val_equal(ff[[2]],as.data.frame(matrix(0,nrow=nrow(test_data),ncol=0)))

  expect_val_equal(ff[[3]],data.frame(model.frame(~x1-1,test_data)))
  expect_colnames_equal(ff[[3]],"x1")

  expect_val_equal(ff[[4]],data.frame(model.frame(~x3-1,test_data)))
  expect_colnames_equal(ff[[4]],"x3")

  expect_val_equal(ff[[5]],data.frame(model.frame(~x1-1,test_data)))
  expect_colnames_equal(ff[[5]],"x1")

  expect_val_equal(ff[[6]],data.frame(model.frame(~x1-1,test_data)))
  expect_colnames_equal(ff[[6]],"x1")

  expect_val_equal(ff[[7]],data.frame(model.frame(~x1+x2-1,test_data)))
  expect_colnames_equal(ff[[7]],c("x1","x2"))

  expect_val_equal(ff[[8]],data.frame(model.frame(~x1*x2-1,test_data)))
  expect_colnames_equal(ff[[8]],c("x1","x2"))

  expect_val_equal(ff[[9]],data.frame(model.frame(~I(x1^2)+I(exp(x2))-1,test_data)))
  expect_colnames_equal(ff[[9]],c("I(x1^2)","I(exp(x2))"))

  expect_val_equal(ff[[10]],data.frame(model.frame(~x1+x3-1,test_data)))
  expect_colnames_equal(ff[[10]],c("x1","x3"))

  expect_val_equal(ff[[11]],data.frame(model.frame(~poly(x1,2)-1,test_data)))
  expect_colnames_equal(ff[[11]],"poly(x1, 2)")



  # model.matrix
  return_type<- "model.matrix"
  expect_val_equal(ff[[1]],as.data.frame(matrix(0,nrow=nrow(test_data),ncol=0)))

  expect_val_equal(ff[[2]],as.data.frame(matrix(0,nrow=nrow(test_data),ncol=0)))

  expect_val_equal(ff[[3]],data.frame(model.matrix(~x1-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[3]],"x1")

  expect_val_equal(ff[[4]],data.frame(model.matrix(~x3-1,test_data),row.names=NULL)[,-1])
  expect_colnames_equal(ff[[4]],c("x32","x33","x34","x35"))

  expect_val_equal(ff[[5]],data.frame(model.matrix(~x1-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[5]],"x1")

  expect_val_equal(ff[[6]],data.frame(model.matrix(~x1-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[6]],"x1")

  expect_val_equal(ff[[7]],data.frame(model.matrix(~x1+x2-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[7]],c("x1","x2"))

  expect_val_equal(ff[[8]],data.frame(model.matrix(~x1*x2-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[8]],c("x1","x2","x1:x2"))

  expect_val_equal(ff[[9]],data.frame(model.matrix(~I(x1^2)+I(exp(x2))-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[9]],c("I(x1^2)","I(exp(x2))"))

  expect_val_equal(ff[[10]],data.frame(model.matrix(~x1+x3-1,test_data),row.names=NULL)[,-2])
  expect_colnames_equal(ff[[10]],c("x1","x32","x33","x34","x35"))

  expect_val_equal(ff[[11]],data.frame(model.matrix(~poly(x1,2)-1,test_data),row.names=NULL))
  expect_colnames_equal(ff[[11]],paste0("poly(x1, 2)",c("1","2")))


})

# names_from_formula
test_that("Covariate names from formula",{
  expect_val_equal<- function(formula,expected) {
    eval(bquote(
      expect_identical(names_from_formula(.(formula)),expected)
    ))
  }
  ff<- c(
    y ~ 1, # 1
    y ~ x, # 2
    y ~ x + time(t), # 3
    y ~ x + space("matern32"), # 4
    y ~ x + time(t) + space("matern32"), # 5
    y ~ x + I(x^2), # 6
    y ~ poly(x,2), # 7
    y ~ x + z, # 8
    y ~ x:z, # 9
    y ~ I(log(x)) + I(sqrt(z)), # 10
    y ~ x + t + time(t), # 11
    y ~ time(t) # 12
  )

  expect_val_equal(ff[[1]],character(0))
  expect_val_equal(ff[[2]],"x")
  expect_val_equal(ff[[3]],"x")
  expect_val_equal(ff[[4]],"x")
  expect_val_equal(ff[[5]],"x")
  expect_val_equal(ff[[6]],"x")
  expect_val_equal(ff[[7]],"x")
  expect_val_equal(ff[[8]],c("x","z"))
  expect_val_equal(ff[[9]],c("x","z"))
  expect_val_equal(ff[[10]],c("x","z"))
  expect_val_equal(ff[[11]],c("x","t"))
  expect_val_equal(ff[[12]],character(0))
})

# response_from_formula
test_that("Response variable from formula",{
  expect_val_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(response_from_formula(.(formula),test_data),expected,ignore_attr=TRUE)
    ))
  }
  expect_name_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(attributes(response_from_formula(.(formula),test_data))$name,expected)
    ))
  }

  test_data<- sf::st_sf(
    y = rnorm(nt*npert),
    response = rnorm(nt*npert),
    t = rep(seq(nt),each=npert),
    geom = sf::st_sample(bbox,nt*npert)
  )
  ff<- c(
    ~ time(t), # 1
    y ~ 1, # 2
    response ~ 1, # 3
    geom ~ 1, # 4
    cbind(y,response) ~ 1, # 5
    c(y,response) ~ 1, # 6
    y+response ~ 1, # 7
    cbind(y,q) ~ 1 # 8
  )

  eval(bquote(expect_error(response_from_formula(ff[[1]],test_data),"Response variable")))

  f<- ff[[2]]
  expect_val_equal(f,data.frame(y=test_data$y))
  expect_name_equal(f,"y")

  f<- ff[[3]]
  expect_val_equal(f,data.frame(response=test_data$response))
  expect_name_equal(f,"response")

  f<- ff[[4]]
  eval(bquote(expect_error(response_from_formula(.(f),test_data),"Response variable")))

  f<- ff[[5]]
  expect_val_equal(f,data.frame(y=test_data$y,response=test_data$response))
  expect_name_equal(f,c("y","response"))

  f<- ff[[6]]
  eval(bquote(expect_error(response_from_formula(.(f),test_data),"In LHS of formula")))

  f<- ff[[7]]
  eval(bquote(expect_error(response_from_formula(.(f),test_data),"In LHS of formula")))

  f<- ff[[8]]
  eval(bquote(expect_error(response_from_formula(.(f),test_data),"Response variable")))
})

# sample_size_from_formula
test_that("Sample size variable from formula",{
  expect_val_equal<- function(formula,unique_vars,expected) {
    eval(bquote(
      expect_equal(sample_size_from_formula(.(formula),test_data,unique_vars),expected)
    ))
  }
  test_data<- sf::st_sf(
    y1 = rnorm(nt*npert),
    y2 = rnorm(nt*npert),
    x1 = rnorm(nt*npert),
    x2 = rnorm(nt*npert),
    geom = sf::st_sample(bbox,nt*npert)
  )
  ff<- c(
    y1 ~ 1, # 1
    y1 ~ sample.size(NA), # 2
    y1 ~ sample.size(1), # 3
    y1 ~ sample.size(2), # 4
    y1 ~ sample.size(x1), # 5
    y1 ~ sample.size(x1+x2), # 6
    y1 ~ sample.size(x1) + sample.size(x2), # 7
    y1 ~ sample.size(nothere), # 8
    cbind(y1,y2) ~ sample.size(1), # 9
    cbind(y1,y2) ~ sample.size(x1), # 10
    cbind(y1,y2) ~ sample.size(cbind(NA,1)), # 11
    cbind(y1,y2) ~ sample.size(cbind(NA,x1)), # 12
    cbind(y1,y2) ~ sample.size(cbind(2,x1)), # 13
    cbind(y1,y2) ~ sample.size(cbind(x1,x1)), # 14
    cbind(y1,y2) ~ sample.size(cbind(x1,x2)), # 15
    cbind(y1,y2) ~ sample.size(cbind(x1,nothere)) # 16
  )

  f<- ff[[1]]
  expect_val_equal(f,TRUE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=0)))
  expect_val_equal(f,FALSE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=1)))

  f<- ff[[2]]
  expect_val_equal(f,TRUE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=0)))
  expect_val_equal(f,FALSE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=1)))

  f<- ff[[3]]
  expect_val_equal(f,TRUE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=0)))
  expect_val_equal(f,FALSE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=1)))

  f<- ff[[4]]
  expect_val_equal(f,TRUE,as.data.frame(matrix(2,nrow=nrow(test_data),ncol=0)))
  expect_val_equal(f,FALSE,as.data.frame(matrix(2,nrow=nrow(test_data),ncol=1)))

  f<- ff[[5]]
  expect_val_equal(f,TRUE,data.frame(x1=test_data$x1))
  expect_val_equal(f,FALSE,data.frame(x1=test_data$x1))

  f<- ff[[6]]
  eval(bquote(expect_error(sample_size_from_formula(.(f),test_data),"In sample.size term")))

  f<- ff[[7]]
  eval(bquote(expect_error(sample_size_from_formula(.(f),test_data),"Multiple `sample.size`")))

  f<- ff[[8]]
  eval(bquote(expect_error(sample_size_from_formula(.(f),test_data),"Sample.size variable")))

  f<- ff[[9]]
  expect_val_equal(f,TRUE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=0)))
  expect_val_equal(f,FALSE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=2)))

  f<- ff[[10]]
  expect_val_equal(f,TRUE,data.frame(x1=test_data$x1))
  expect_val_equal(f,FALSE,data.frame(x1=test_data$x1,x1=test_data$x1,check.names=FALSE))

  f<- ff[[11]]
  expect_val_equal(f,TRUE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=0)))
  expect_val_equal(f,FALSE,as.data.frame(matrix(1,nrow=nrow(test_data),ncol=2)))

  f<- ff[[12]]
  expect_val_equal(f,TRUE,data.frame(x1=test_data$x1))
  expect_val_equal(f,FALSE,data.frame(V1=1,x1=test_data$x1))

  f<- ff[[13]]
  expect_val_equal(f,TRUE,data.frame(x1=test_data$x1))
  expect_val_equal(f,FALSE,data.frame(V1=2,x1=test_data$x1))

  f<- ff[[14]]
  expect_val_equal(f,TRUE,data.frame(x1=test_data$x1))
  expect_val_equal(f,FALSE,data.frame(x1=test_data$x1,x1=test_data$x1,check.names=FALSE))

  f<- ff[[15]]
  expect_val_equal(f,TRUE,data.frame(x1=test_data$x1,x2=test_data$x2))
  expect_val_equal(f,FALSE,data.frame(x1=test_data$x1,x2=test_data$x2))

  f<- ff[[16]]
  eval(bquote(expect_error(sample_size_from_formula(.(f),test_data),"Sample.size variable")))
})

# time_from_formula & time_name
test_that("Time information from formula",{
  expect_time_val_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(time_from_formula(.(formula),test_data,return="column"),expected,ignore_attr=T)
    ))
  }
  expect_time_type_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(attributes(time_from_formula(.(formula),test_data,return="column"))$type,.(expected))
    ))
  }
  expect_time_name_equal<- function(formula,expected) {
    eval(bquote(
      expect_equal(colnames(time_from_formula(.(formula),test_data,return="column")),.(expected))
    ))
  }

  test_data<- sf::st_sf(
    t = rep(seq(nt),each=npert),
    year = rep(-seq(nt),each=npert),
    geom = sf::st_sample(bbox,nt*npert)
  )
  ff<- c(
    y ~ time(t), # 1
    y ~ time(t,"ar1"), # 2
    y ~ time(t,"rw"), # 3
    y ~ time(t,"independent"), # 4
    y ~ time(year), # 5
    y ~ time(geom), # 6
    y ~ time(month), # 7
    y ~ time(t) + time(year), # 8
    y ~ time(t+year), # 9
    y ~ 1, # 10
    cbind(y1,y2) ~ time(t), # 11
    cbind(y1,y2) ~ time(t,c("ar1","rw")) # 12
  )

  f<- ff[[1]]
  expect_time_val_equal(f,data.frame(t=test_data$t))
  expect_time_type_equal(f,"ar1")
  expect_time_name_equal(f,"t")
  eval(bquote(
    expect_match(time_name(.(f)),"t")
  ))

  f<- ff[[2]]
  expect_time_val_equal(f,data.frame(t=test_data$t))
  expect_time_type_equal(f,"ar1")
  expect_time_name_equal(f,"t")
  eval(bquote(
    expect_match(time_name(.(f)),"t")
  ))

  f<- ff[[3]]
  expect_time_val_equal(f,data.frame(t=test_data$t))
  expect_time_type_equal(f,"rw")
  expect_time_name_equal(f,"t")
  eval(bquote(
    expect_match(time_name(.(f)),"t")
  ))

  f<- ff[[4]]
  expect_time_val_equal(f,data.frame(t=test_data$t))
  expect_time_type_equal(f,"independent")
  expect_time_name_equal(f,"t")
  eval(bquote(
    expect_match(time_name(.(f)),"t")
  ))

  f<- ff[[5]]
  expect_time_val_equal(f,data.frame(year=test_data$year))
  expect_time_type_equal(f,"ar1")
  expect_time_name_equal(f,"year")
  eval(bquote(
    expect_match(time_name(.(f)),"year")
  ))

  f<- ff[[6]]
  eval(bquote(
    expect_error(time_from_formula(.(f),test_data),
                 "Time variable must be numeric.")
  ))

  f<- ff[[7]]
  eval(bquote(
    expect_error(time_from_formula(.(f),test_data),"Time variable month")
  ))

  f<- ff[[8]]
  eval(bquote(
    expect_error(time_from_formula(.(f),test_data),"Multiple `time` terms")
  ))

  f<- ff[[9]]
  eval(bquote(
    expect_error(time_from_formula(.(f),test_data),"Must supply exactly one")
  ))

  f<- ff[[10]]
  expect_time_val_equal(f,data.frame(Time=numeric(nrow(test_data))))
  expect_time_type_equal(f,"independent")
  expect_time_name_equal(f,"Time")
  eval(bquote(
    expect_match(time_name(.(f)),"Time")
  ))

  f<- ff[[11]]
  expect_time_val_equal(f,data.frame(t=test_data$t))
  expect_time_type_equal(f,c("ar1","ar1"))
  expect_time_name_equal(f,"t")
  eval(bquote(
    expect_equal(time_name(.(f)),"t")
  ))

  f<- ff[[12]]
  expect_time_val_equal(f,data.frame(t=test_data$t))
  expect_time_type_equal(f,c("ar1","rw"))
  expect_time_name_equal(f,"t")
  eval(bquote(
    expect_equal(time_name(.(f)),"t")
  ))
})
