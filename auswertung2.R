library(Hmisc)

source("multiexpfit.R")
source("gausfit.R")

messung=read.table("data/Hauptmessung.TKA")

rebinfactor=2
rebinned=c()

for(i in c(1:(1024/rebinfactor))){
  rebinned[i]=0
  for(j in c(0:(rebinfactor-1))){
    rebinned[i] = rebinned[i]+ messung[[1]][i*rebinfactor-1+j]
  }
  
}



bereich=c(50,1024/rebinfactor)
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

daten=data1
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()

par(mfrow=c(1,1))

bereich_bins=c(75,275)
bereich=bereich_bins-50
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
printfitdata(fit)

result=hist(multiexpfit$A,breaks=50,plot=FALSE)
daten=data.frame(x=result$mids,y=result$density,sy=sqrt(result$density))
#cat("\n")
#print(daten)
bereich=c(which.min(daten$x),which.max(daten$x))
plot(result$mids,result$density,type="h",lwd=10,col="gray30",xlab="Zerfallskonstante",ylab="Counts",bty="l")
fitA=gausfit(daten,bereich,FALSE)
#plotgaus(fit,c(daten$x[bereich[1]],daten$x[bereich[2]]))
printfitdata(fitA)

result=hist(multiexpfit$C,breaks=50,plot=FALSE)
daten=data.frame(x=result$mids,y=result$density,sy=sqrt(result$density))
#cat("\n")
#print(daten)
bereich=c(which.min(daten$x),which.max(daten$x))
#plot(result$mids,result$density,type="h",lwd=10,col="gray30",xlab="Zerfallskonstante",ylab="Counts",bty="l")
fitC=gausfit(daten,bereich,FALSE)
#plotgaus(fit,c(daten$x[bereich[1]],daten$x[bereich[2]]))
printfitdata(fitC)


fitData=data.frame(matrix(vector(),1,2,dimnames=list(c("lambda"),c("Estimate","Std. Error"))),stringsAsFactors=FALSE)
fitData["lambda","Estimate"]=fit["mu","Estimate"]
fitData["lambda","Std. Error"]=fit["mu","Std. Error"]
fitData["A","Estimate"]=fitA["mu","Estimate"]
fitData["C","Estimate"]=fitC["mu","Estimate"]

printexpdata(fitData,title="Werte für den 14,4 keV-Zustand von 57Fe",factor=0.58,error=0.05)

daten=data1
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()

plotexp(fitData,bereich_bins)
grid()
