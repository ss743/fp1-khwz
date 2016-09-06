library(Hmisc)

messung=read.table("data/Hauptmessung.TKA")

rebinfactor=1
rebinned=c()
startabschnitt=25

for(i in c(1:(1024/rebinfactor))){
  rebinned[i]=0
  for(j in c(0:(rebinfactor-1))){
    rebinned[i] = rebinned[i]+ messung[[1]][i*rebinfactor-1+j]
  }
}

bereich=c(startabschnitt,1024/rebinfactor)
#bereich2=c(50,1024)

messung1=rebinned[bereich[1]:bereich[2]]
data1=data.frame(x=bereich[1]:bereich[2],y=messung1,sy=sqrt(messung1))

