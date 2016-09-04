source("expfit.R")

multiexpfit <- function(input,bereich,width,weighted=FALSE){
  count=0
  total=0
  lambda=c()
  error=c()
  for (i in bereich[1]:(bereich[2]-width)){
    for (j in i+width:bereich[2]){
      total=total+1
      result = tryCatch({
        fit=expfit(input,c(i,j),weighted)
        #printexpdata(fit)
        l=fit["lambda","Estimate"]
        if(!is.na(l)){
          count=count+1
          lambda[count]=l
          error[count]=fit["lambda","Std. Error"]
        }
      }, warning = function(w) {
        cat("Nich so gut:")
        print(w)
      }, error = function(e) {
        #cat("Ziemlich doof:")
        #print(e)
      }, finally = {
        #cat("\n")
      })
    }
  }
  cat("Gewollte Fits: ")
  cat(total)
  cat("\nGemachte Fits: ")
  cat(count)
  return(data.frame(lambda,error))
}