library(Hmisc)

source("multiexpfit.R")
source("gausfit.R")

messung=read.table("data/Hauptmessung.TKA")

bereich=c(50,1024)

messung1=messung[[1]][bereich[1]:bereich[2]]
data1=data.frame(x=bereich[1]:bereich[2],y=messung1,sy=sqrt(messung1))

par(mfrow=c(1,1))

plottype <- "p"
pointsize<- 0.6

weighted <- TRUE



daten=data1
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()

bereich=c(100,200)
bereich=bereich-50
fit=multiexpfit(daten,bereich,50,weighted)
result=hist(fit$lambda,breaks=50,plot=FALSE)
daten=data.frame(x=result$mids,y=result$density,sy=sqrt(result$density))
cat("\n")
#print(daten)
bereich=c(which.min(daten$x),which.max(daten$x))
plot(result$mids,result$density,type="h",lwd=10,col="gray30",xlab="Zerfallskonstante",ylab="Counts",bty="l")
fit=gausfit(daten,bereich,FALSE)
#plotgaus(fit,bereich)
#printfitdata(fit)
grid()
