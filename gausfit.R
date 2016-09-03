gausfit <- function(input,bereich,weighted=FALSE){
  
  thegaussian <- y ~ C + N*exp(-(x-mu)^2/(2*sig^2))
  
  daten=input[bereich[1]:bereich[2],]
  
  ymin=min(daten$y)
  ymax=max(daten$y)
  mu0 =daten$x[which.max(daten$y)]
  sig0=(bereich[2]-bereich[1])/3
  err=daten$sy
  
  if(weighted)
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  
  return(summary(fit)$parameters)
  
}

plotgaus <- function(fitdata,bereich){
  
  N<-fitdata["N","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  plot (function(x){C + N*exp(-(x-mu)^2/(2*sig^2))},bereich[1],bereich[2],add=T,col="red")
  
}

printfitdata <- function(fitdata,title=""){
  
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  sig<-fitdata["sig","Estimate"]
  ssig<-fitdata["sig","Std. Error"]
  
  cat(title)
  cat("\n")
  
  cat(" mu    = ")
  cat(mu)
  cat("+-")
  cat(smu)
  cat("\n")
  
  cat(" sigma = ")
  cat(sig)
  cat("+-")
  cat(ssig)
  cat("\n")
  #cat("\n")
  
}
