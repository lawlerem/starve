library(staRVe)

simPars<- new("staRVe_parameters")
covariance_function(simPars)<- "exponential"
spatial_parameters(simPars)[c("sd","range"),"par"]<- c(1,0.4)
time_parameters(simPars)[c("mu"),"par"]<- c(0)
time_parameters(simPars)[c("ar1","sd"),"fixed"]<- c(T,T) # Spatial only
response_distribution(simPars)<- "gaussian"
response_parameters(simPars)[c("sd"),"par"]<- c(0.5)


locs<- expand.grid(
  x = seq(0,1,length.out=25),
  y = seq(0,1,length.out=25)
) |> st_as_sf(coords=c("x","y"))
locs$y<- 0
locs$t<- 0

simModel<- prepare_staRVe_model(
  y~time(t),
  locs
)


simRefit<- function(
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
    fit = T
  )
  pred<- staRVe_predict(simFit,dat(simDat))
  return(list(
    simulated = simDat,
    fitted = simFit,
    predicted = pred
  ))
}

plotSim<- function(x) {
  par(mfrow=c(2,2))
  plot(x$predicted[,"w"],main = "Predicted",pch=20,cex=2,key.pos=NULL,reset=F)
  plot(x$predicted[,"w_se"],main = "Std. Error",pch=20,cex=2,key.pos=NULL,reset=F)
  plot(dat(x$simulated)[,"w"],main = "Simulated",pch=20,cex=2,key.pos=NULL,reset=F)
  plot(dat(x$fitted)[,"y"],main = "Data",pch=20,cex=2,key.pos=NULL,reset=F,xlim=c(0,1),ylim=c(0,1))
}


foo<- simRefit(simModel,simPars,100,seed=4326)
plotSim(foo)
