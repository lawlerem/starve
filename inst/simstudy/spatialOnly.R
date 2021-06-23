library(staRVe)

simPars<- new("staRVe_parameters")
covariance_function(sim_pars)<- "exponential"
spatial_parameters(simPars)[c("sd","range"),"par"]<- c(1,0.4)
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

parameters(simModel)<- simPars

simDat<- staRVe_simulate(simModel)





distSort<- function(d) {
  for(i in 1:(ncol(d)-1) ) {
    myorder<- order(colSums(d[1:i,-(1:i),drop=F]))+i
    d[(i+1):ncol(d),(i+1):ncol(d)]<- d[myorder,myorder]
  }
  return(d)
}
