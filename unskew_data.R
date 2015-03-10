#http://rstudio-pubs-static.s3.amazonaws.com/1563_1ae2544c0e324b9bb7f6e63cf8f9e098.html

library(e1071)

skew_score <- function(c, x) (skewness(log(x + c)))^2

change_skew <- function(x){
  opt_result <- optimise(skew_score, c(1e-5, 20), x = x)
  best.c <- opt_result$minimum
  #best.s <- opt_result$objective
  log(x + best.c)
}

imgTrainDT[, names(imgTrainDT):=lapply(.SD, change_skew), .SDcols=1:ncol(imgTrainDT)]



#dd <- imgTrainDT[, V1]
#print(skewness(dd))

#hist(dd)
#hist(log(dd+best.c))
#hist
