library(staRVe)

oneSimRefit<- function(
  simTemplate,
  simPars,
  nLocs,
  seed = NA
) {
  if( !is.na(seed) ) set.seed(seed)
  parameters(simModel)<- simPars
  simDat<- staRVe_simulate(simModel)
  simFit<- prepare_staRVe_model(
    formula(simDat),
    dat(simDat)[c(1,sample(nrow(dat(simDat)),size=nLocs)),],
    distribution = response_distribution(simPars),
    fit = F
  )
  parameters(simFit)<- simPars
  simFit<- staRVe_fit(simFit,silent=T)
  pred<- staRVe_predict(simFit,dat(simDat))
  pred<- st_join(
    random_effects(simDat)[,"w"],
    pred
  )
  colnames(pred)[1:2]<- c("sim_w","w")
  return(list(
    simulated = simDat,
    fitted = simFit,
    predicted = pred
  ))
}

multiRefitSummary<- function(simTemplate,simPars,nLocs,nsim = 1,ncores=1) {
  start_time<- Sys.time()
  # foo<- data.frame(
  #   space_sd = rep(NA,nsim+1),
  #   space_sd_se = NA,
  #   range = NA,
  #   range_se = NA,
  #   mu = NA,
  #   mu_se = NA,
  #   obs_sd = NA,
  #   obs_sd_se = NA,
  #   w_coverage = NA,
  #   loglikelihood = NA,
  #   convergence = NA,
  #   timing = NA,
  #   seed = NA
  # )
  # foo[1,c("space_sd","range","mu","obs_sd")]<- c(
  #   spatial_parameters(simPars)[c("sd","range"),"par"],
  #   time_parameters(simPars)["mu","par"],
  #   response_parameters(simPars)["sd","par"]
  # )
  refitSummary<- do.call(rbind,mclapply(seq(nsim),function(i) {
    cat(paste0(i," / ",nsim,"; Time Elapsed: ")); print(Sys.time()-start_time)
    seed<- sample(100000,1)
    foo<- data.frame(
      space_sd = NA,
      space_sd_se = NA,
      range = NA,
      range_se = NA,
      mu = NA,
      mu_se = NA,
      obs_sd = NA,
      obs_sd_se = NA,
      w_coverage = NA,
      loglikelihood = NA,
      convergence = NA,
      timing = NA,
      seed = seed
    )
    try({
      time<- system.time({
        sim<- oneSimRefit(simTemplate,simPars,nLocs,seed)
      })
      fitted<- sim$fitted
      foo[1,c("space_sd","space_sd_se")]<- spatial_parameters(fitted)["sd",c("par","se")]
      foo[1,c("range","range_se")]<- spatial_parameters(fitted)["range",c("par","se")]
      foo[1,c("mu","mu_se")]<- time_parameters(fitted)["mu",c("par","se")]
      foo[1,c("obs_sd","obs_sd_se")]<- response_parameters(fitted)["sd",c("par","se")]
      foo[1,"w_coverage"]<- mean(apply(sim$predicted,MARGIN=1,function(row) {
        return(with(row,w-1.96*w_se < sim_w & sim_w < w+1.96*w_se))
      }))
      foo[1,"loglikelihood"]<- c(obj(TMB_out(fitted))$fn())
      foo[1,"convergence"]<- convergence(fitted)
      foo[1,"timing"]<- time[["elapsed"]]
    })
    return(foo)
  },mc.cores=ncores))
  return(refitSummary)
}

plotSim<- function(x) {
  par(mfrow=c(2,2))
  plot(x$predicted[,"w"],main = "Predicted",pch=20,cex=1.5,key.pos=NULL,reset=F)
  plot(x$predicted[,"w_se"],main = "Std. Error",pch=20,cex=1.5,key.pos=NULL,reset=F)
  plot(dat(x$simulated)[,"w"],main = "Simulated",pch=20,cex=1.5,key.pos=NULL,reset=F)
  plot(dat(x$fitted)[,"y"],main = "Data",pch=20,cex=1.5,key.pos=NULL,reset=F,xlim=c(0,1),ylim=c(0,1))
}




simPars<- new("staRVe_parameters")
covariance_function(simPars)<- "exponential"
spatial_parameters(simPars)[c("sd","range"),"par"]<- c(1,0.4)
spatial_parameters(simPars)[c("sd","range"),"fixed"]<- c(F,T)
time_parameters(simPars)[c("mu"),"par"]<- c(0)
time_parameters(simPars)[c("ar1","sd"),"fixed"]<- c(T,T) # Spatial only
response_distribution(simPars)<- "gaussian"
response_parameters(simPars)[c("sd"),"par"]<- c(0.5)


locs<- expand.grid(
  x = seq(0,1,length.out=20),
  y = seq(0,1,length.out=20)
) |> st_as_sf(coords=c("x","y"))
locs$y<- 0
locs$t<- 0

simModel<- prepare_staRVe_model(
  y~time(t),
  locs
)


summary<- multiRefitSummary(
  simModel,
  simPars,
  nLocs = 100,
  nsim = 20,
  ncores = 5
)

foo<- oneSimRefit(simModel,simPars,100,seed=4326)
plotSim(foo)
