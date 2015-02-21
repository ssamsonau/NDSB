# multiclass logLoss
# https://www.kaggle.com/wiki/MultiClassLogLoss

mcLogLoss <- function(actual.c, predicted.prob.DT){
  # actual.c - vector with actual classes for all examples
  
  # predicted.prob.DT - Data table with col names showing classes
  # rows contain info for prob being in certain class for give example
  mlogLoss <- 0
  
  
  for(i in  1:length(actual.c)){
    j <- which(names(predicted.prob.DT) == actual.c[i])
    prob_ij <- as.numeric( predicted.prob.DT[i, j, with=F] ) 
    mlogLoss <- mlogLoss + log(prob_ij)
    cat(i, j, mlogLoss)
  }
  
  numClasses <- nrow(predicted.prob.DT) 
  
  mlogLoss <- -mlogLoss/numClasses
  mlogLoss  
}