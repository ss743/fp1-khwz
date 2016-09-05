konstfit <- function(input,bereich){
  
  daten=input[bereich[1]:bereich[2],]
  a=sum(daten$y)/(bereich[2]-bereich[1])
  
  return(c(a,a))
  
}

konsterror <- function(input,bereich,mittelwert){
  
  daten=input[bereich[1]:bereich[2],]
  n=bereich[2]-bereich[1]
  einheit=c(bereich[1]:bereich[2])/c(bereich[1]:bereich[2])
  a=sqrt(1/((n-1))*sum((daten$y-mittelwert*einheit)^2))
  
  return(a)
  
}