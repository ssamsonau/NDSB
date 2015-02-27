mcLogloss_metrics <- function(data, lev = NULL, model = NULL){
  
  source("mcLogLoss.R")
   
  out <- mcLogLoss(actual.c = data[, "obs"],  
                   predicted.prob.DF = subset(data, select=-c(pred, obs) ) )
    
  names(out) <- c("mcLogloss")
  
  out
  
  
}    