# multiclass logLoss
# https://www.kaggle.com/wiki/MultiClassLogLoss

mcLogLoss <- function(actual.c, predicted.prob.DF, ignore.Inf = FALSE){
  # actual.c - vector with actual classes for all examples
  
  # predicted.prob.DF - Data frame with col names showing classes
  # rows contain info for prob being in certain class for give example
  
  i <- 1:length(actual.c) # given example number
  j <- match(actual.c, names(predicted.prob.DF)) # for a give example j shows 
                                                 # a position of predicted probablitiy
                                                 # to be in class where is should be
  ij <- cbind(i, j)  
  mat <- as.matrix(predicted.prob.DF)
  log.mat <- log(mat[ij])
  
  if(ignore.Inf)  log.mat[is.infinite(log.mat)] <- 0
  
  mlogLoss <- sum( log.mat )
    
  numObserv <- length(actual.c) 
  
  mlogLoss <- -mlogLoss/numObserv
  mlogLoss  
}