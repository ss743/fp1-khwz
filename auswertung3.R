#---Einbinden der Libraries
library(Hmisc)
source("gausfit.R")
#---Einbinden der Libraries

#---Einlesen der Messdaten
messung=read.table("data/Hauptmessung.TKA")
#---Einlesen der Messdaten

startabschnitt=50 #Vernachlässigen der ersten 50 Channels

#############
# Rebinning #
#############
rebinfactor=1   # => Kein Rebinning, da Linearfits auch trotz großer Streuung funktionieren
rebinned=c()

if(rebinfactor>1)
{
  for(i in c(1:(1024/rebinfactor))){
    rebinned[i]=0
    for(j in c(0:(rebinfactor-1))){
      rebinned[i] = rebinned[i]+ messung[[1]][i*rebinfactor-1+j]
    }
  }
} else
{
  rebinned=messung[[1]]  
}

#---Speichern der Daten in Variablen
bereich=c(startabschnitt,1024/rebinfactor)
messung1=rebinned[bereich[1]:bereich[2]]
data1=data.frame(x=bereich[1]:bereich[2],y=log(messung1-28),sy=sqrt(messung1)) #Abzug des Untergrundes
data2=data.frame(x=bereich[1]:bereich[2],y=log(messung1),sy=sqrt(messung1))
#---Speichern der Daten in Variablen

#---Festlegung des Intervalls
int=c(80,520)/rebinfactor-startabschnitt
#---Festlegung des Intervalls

###################
# Multi-Linearfit #
###################
daten=data1
xdata=daten$x[int[1]:int[2]]
ydata=daten$y[int[1]:int[2]]

slopes=c()
intercepts=c()
err=c()
i_err=c()
c=0
width=50
for(i in c(int[1]:(int[2]-width))){
  #print(i)
  for(j in c((i+width):int[2])){
    c=c+1
    x=xdata[i:j]
    y=ydata[i:j]
    fm <- lm(y~x)
    
    intercept=fm$coefficients["(Intercept)"]
    slope=fm$coefficients["x"]
    error=summary(fm)$coefficients["x","Std. Error"]
    i_error=summary(fm)$coefficients["(Intercept)","Std. Error"]
    slopes[c]=slope
    intercepts[c]=intercept
    err[c]=error
    i_err[c]=i_error
  }
}

#---Plot des Histogramms und Gauß-Fit
result=hist(slopes,breaks=50,plot=FALSE)
daten=data.frame(x=result$mids,y=result$density,sy=sqrt(result$density))
plot(result$mids,result$density,type="h",lwd=10,col="gray30",xlab="Zerfallskonstante",ylab="Counts",bty="l")
bereich=c(which.min(daten$x),which.max(daten$x))
fit=gausfit(daten,bereich,FALSE,sig0=0.001,N0=2400)
plotgaus(fit,c(daten$x[bereich[1]],daten$x[bereich[2]]))
#---Plot des Histogramms und Gauß-Fit

############################
# Ergebnisse des Gauß-Fits #
############################
mu=fit["mu","Estimate"]
sigma=sqrt((fit["sig","Estimate"])^2)

cat("Ergebnis:\n lambda = ")
cat(mu)
cat("+-")
cat(sigma)
cat(" 1/Channel")

##############################################
# Suche alle Datensätze zum mittleren lambda #
##############################################
count=0
As=c()
As_err=c()
for(i in c(1:length(slopes)))
{
  if(mu-sigma/50 < slopes[i] && slopes[i] < mu+sigma/50)
  {
    count=count+1
    As[count]=intercepts[i]
    As_err[count]=i_err[i]
  }
}

A=sum(As)/count
A_err=sqrt(sum(As_err^2))/count

intercept=A
slope=mu

#########
# Plots #
#########
daten=data2
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="log(Counts)",cex=pointsize,bty="l")
grid()

daten=data1
plot(daten$x,daten$y,type=plottype,pch=4,xlab="Channel",ylab="log(Counts)",cex=pointsize,bty="l")
grid()
redline<-data.frame(x=xdata,y=intercept+slope*xdata)

lines(redline,col="red",xlim=int)
