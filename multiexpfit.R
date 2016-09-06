source("expfit.R")

multiexpfit <- function(input,bereich,width,weighted=FALSE){
  count=0
  count_fehl=0
  total=0
  lambda=c()
  A=c()
  C=c()
  A_err=c()
  C_err=c()
  error=c()
  breite=c()
  breite_fehl=c()
  bin_counts=rep(0,bereich[2])
  bin_counts_fehl=rep(0,bereich[2])
  for (i in bereich[1]:(bereich[2]-width)){
    for (j in (i+width):bereich[2]){
      #cat("\ni: ")
      #cat(i)
      #cat(", j: ")
      #cat(j)
      total=total+1
      breite[total]=j-i
      succeeded=FALSE
      bin_counts[i:j]=bin_counts[i:j]+1
      result = tryCatch({
        fit=expfit(input,c(i,j),weighted)
        succeeded=TRUE
        #printexpdata(fit)
        l=fit["lambda","Estimate"]
        if(!is.na(l)){
          count=count+1
          lambda[count]=l
          error[count]=fit["lambda","Std. Error"]
          A[count]=fit["A","Estimate"]
          A_err[count]=fit["A","Std. Error"]
          C[count]=fit["C","Estimate"]
          C_err[count]=fit["C","Std. Error"]
        }
      }, warning = function(w) {
        cat("Nich so gut:")
        print(w)

      }, error = function(e) {
        #cat("Ziemlich doof:")
        #print(e)
        #cat("\nStart-Bin: ")
        #cat(i)
        #cat("\nStop-Bin:")
        #cat("\n")
    }, finally = {
        #cat("\n")
      if(!succeeded){
        count_fehl=count_fehl+1
        #print(count_fehl)
        breite_fehl[count_fehl]=j-i
        bin_counts_fehl[i:j]=bin_counts_fehl[i:j]+1
      }
      
      })
    }
  }
  cat("Gewollte Fits: ")
  cat(total)
  cat("\nGemachte Fits: ")
  cat(count)
  
  #h1=hist(breite)
  #print(breite_fehl)
  #h2=hist(breite_fehl,add=TRUE)
  
  #plot(h1$mid,h2$density/h1$density,type="h")
  #plot(daten$x[1]:daten$x[bereich[2]],bin_counts_fehl/bin_counts)
  
  
  return(data.frame(lambda,error,A,C,A_err,C_err))
}