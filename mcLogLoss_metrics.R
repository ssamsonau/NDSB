mcLogloss_metrics <- function(data, lev = NULL, model = NULL){
  
  source("mcLogLoss.R")
   
  out <- mcLogLoss(actual.c = data[, "obs"],  
                   predicted.prob.DF = subset(data, select=-c(pred, obs) ), ignore.Inf=TRUE )
    
  names(out) <- c("mcLogloss")
  
  out
  
  
}    