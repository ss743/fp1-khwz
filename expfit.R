expfit <- function(input, bereich, weighted=FALSE){ #--- Fitten der Exponentialfunktion
  
  theexponential <- y ~ A + C *exp(lambda*x)
  
  daten=input[bereich[1]:bereich[2],]
  
  #---Startwerte
  ymin=min(daten$y)
  xmin=daten$x[which.min(daten$y)]
  ymax=max(daten$y)
  xmax=daten$x[which.max(daten$y)]
  lambda_est=log(2)/200
  C_est=ymin*exp(-lambda_est*xmin)
  A_est=0
  err=daten$sy
  startvalues=list(C=C_est,A=A_est,lambda=lambda_est)
  #---Startwerte
  
  #---Durchführen des Fits
  nlc<-nls.control(maxiter=5000)
  if(weighted)
    fit = nls(theexponential,daten,weights=1/err^2,start=startvalues,control=nlc)
  else
    fit = nls(theexponential,daten,start=startvalues,control=nlc)
  
  return(summary(fit)$parameters)
  
}

plotexp <- function(fitdata,bereich){ #--- Plotten der gefitteten Exponentialfunktion in vorhandenen Graph
  
  lambda<-fitdata["lambda","Estimate"]
  C<-fitdata["C","Estimate"]
  A<-fitdata["A","Estimate"]
  
  plot (function(x){A + C *exp(lambda*x)},bereich[1],bereich[2],add=TRUE,col="red")
  
}

printexpdata <- function(fitdata,title="",factor=1,error=0){ #--- Ausgabe der Exponentialfit-Daten
  
  lambda<-fitdata["lambda","Estimate"]/factor
  slambda<-lambda*sqrt((fitdata["lambda","Std. Error"]/fitdata["lambda","Estimate"])^2+(error/factor)^2)

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
