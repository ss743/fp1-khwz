expfit <- function(input, bereich, weighted=FALSE){
  
  theexponential <- y ~ A + C *exp(lambda*x)
  
  daten=input[bereich[1]:bereich[2],]
  
  ymin=min(daten$y)
  xmin=daten$x[which.min(daten$y)]
  #if(ymin<0) ymin=0
  ymax=max(daten$y)
  xmax=daten$x[which.max(daten$y)]
  lambda_est=log(ymin/ymax)/(xmin-xmax)
  C_est=ymin*exp(-lambda_est*xmin)
  A_est=0
  
  err=daten$sy
  
  startvalues=list(C=C_est,A=A_est,lambda=lambda_est)
  
  if(weighted)
    fit = nls(theexponential,daten,weights=1/err^2,start=startvalues)
  else
    fit = nls(theexponential,daten,start=startvalues)
  
  return(summary(fit)$parameters)
  
}

plotexp <- function(fitdata,bereich){
  
  lambda<-fitdata["lambda","Estimate"]
  C<-fitdata["C","Estimate"]
  A<-fitdata["A","Estimate"]
  
  plot (function(x){A + C *exp(lambda*x)},bereich[1],bereich[2],add=TRUE,col="red")
  
}

printexpdata <- function(fitdata,title=""){
  
  lambda<-fitdata["lambda","Estimate"]
  slambda<-fitdata["lambda","Std. Error"]
  C<-fitdata["C","Estimate"]
  sC<-fitdata["C","Std. Error"]
  A<-fitdata["A","Estimate"]
  sA<-fitdata["A","Std. Error"]
  
  cat(title)
  cat("\n")
  
  cat(" Zerfallskonstante    = ")
  cat(lambda)
  cat("+-")
  cat(slambda)
  cat("\n")
  
  tau=1/lambda
  stau=1/lambda*slambda/lambda
  
  cat(" Mittlere Lebensdauer = ")
  cat(tau)
  cat("+-")
  cat(stau)
  cat("\n")
  
  T=log(2)*tau
  sT=log(2)*stau

  cat(" Halbwertszeit        = ")
  cat(T)
  cat("+-")
  cat(sT)
  cat("\n")
    
}
