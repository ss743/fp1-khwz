source("multiexpfit.R")

par(mfrow=c(1,1))

s=200
x=c(1:200)
y=2*exp(0.05*x)+rnorm(x,sd=s)
for(i in x){
  if(y[i]<0){
    y[i]=0
  }
}
err=(s*x)/x

daten=data.frame(x,y,err)
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
#axis(1,at=c(0:20)*20)
#axis(2,at=c(0:11)*100000000)
grid()

bereich=c(100,200)
fit=multiexpfit(daten,bereich,50,FALSE)
#plotexp(fit,bereich)
#printexpdata(fit)