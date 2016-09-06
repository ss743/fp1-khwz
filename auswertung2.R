library(Hmisc)

source("multiexpfit.R")
source("gausfit.R")
source("konstfit.R")

messung=read.table("data/Hauptmessung.TKA")

rebinfactor=4
rebinned=c()
startabschnitt=25

for(i in c(1:(1024/rebinfactor))){
  rebinned[i]=0
  for(j in c(0:(rebinfactor-1))){
    rebinned[i] = rebinned[i]+ messung[[1]][i*rebinfactor-1+j]
  }
  
}



bereich=c(startabschnitt,1024/rebinfactor)
bereich2=c(50,1024)

messung1=rebinned[bereich[1]:bereich[2]]
data1=data.frame(x=bereich[1]:bereich[2],y=messung1,sy=sqrt(messung1))
messung2=messung[[1]][bereich2[1]:bereich2[2]]
data2=data.frame(x=bereich2[1]:bereich2[2],y=messung2,sy=sqrt(messung2))

par(mfrow=c(1,2))

plottype <- "p"
pointsize<- 0.6

weighted <- TRUE



daten=data2
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()
title("Original (1024 Kanäle)")

daten=data1
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()
title("Rebin (256 Kanäle)")

par(mfrow=c(1,1))
data2
plot(data2$x,data2$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()


bereich_bins=c(120,520)/rebinfactor
bereich=bereich_bins-startabschnitt
fit=multiexpfit(daten,bereich,50,weighted)
result=hist(fit$lambda,breaks=50,plot=FALSE)
daten=data.frame(x=result$mids,y=result$density,sy=sqrt(result$density))
multiexpfit=fit
#cat("\n")
#print(daten)
bereich=c(which.min(daten$x),which.max(daten$x))
plot(result$mids,result$density,type="h",lwd=10,col="gray30",xlab="Zerfallskonstante",ylab="Counts",bty="l")
fit=gausfit(daten,bereich,FALSE)
plotgaus(fit,c(daten$x[bereich[1]],daten$x[bereich[2]]))
#printfitdata(fit)

#Suche alle Datensätze zum mittleren lambda
mu=fit["mu","Estimate"]
sigma=fit["sig","Estimate"]
count=0
As=c()
As_err=c()
Cs=c()
Cs_err=c()
for(i in c(1:length(multiexpfit$lambda)))
{
  if(mu-sigma/5 < multiexpfit$lambda[i] && multiexpfit$lambda[i] < mu+sigma/5)
  {
    count=count+1
    As[count]=multiexpfit$A[i]
    Cs[count]=multiexpfit$C[i]
    As_err[count]=multiexpfit$A_err[i]
    Cs_err[count]=multiexpfit$C_err[i]
  }
}

#print(count)
A=sum(As)/count
C=sum(Cs)/count
A_err=sqrt(sum(As_err^2))/count
C_err=sqrt(sum(Cs_err^2))/count


fitData=data.frame(matrix(vector(),1,2,dimnames=list(c("lambda"),c("Estimate","Std. Error"))),stringsAsFactors=FALSE)
fitData["lambda","Estimate"]=fit["mu","Estimate"]
fitData["lambda","Std. Error"]=fit["sig","Estimate"]
fitData["A","Estimate"]=A
fitData["A","Std. Error"]=A_err
fitData["C","Estimate"]=C
fitData["C","Std. Error"]=C_err
cat("\n")
printexpdata(fitData,title="Werte für den 14,4 keV-Zustand von 57Fe")#,factor=rebinfactor*0.58,error=0.05)

daten=data1
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()

#konstfitbereich=c(420,512)
konstfitbereich=c(840,1024)/rebinfactor
plotexp(fitData,bereich_bins)

lambda<-fitData["lambda","Estimate"]
C<-fitData["C","Estimate"]
A<-fitData["A","Estimate"]


cat("\n")
cat(A)
cat("+")
cat(C)
cat("*exp(")
cat(lambda)
cat("*t)\n")

konst=konstfit(daten,konstfitbereich-startabschnitt)
lines(konstfitbereich,konst,type="l",col="red")
cat("Untergrund\n Gefittet: ")
cat(A)
cat("+-")
cat(A_err)
cat("\n Gemessen: ")
cat(konst[1]/rebinfactor)
cat("+-")
cat(konsterror(daten,konstfitbereich-startabschnitt,konst[1])/rebinfactor)
cat("\nMultiplikative Konstante: ")
cat(C)
cat("+-")
cat(C_err)

#data=daten[420:500,]
#a=sum(data$y)/(80)

#return(c(a,a))

