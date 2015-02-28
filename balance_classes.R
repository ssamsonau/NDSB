Sampling <- function(subDT, size, type){
  if(type=="incr") newDT <- subDT[sample(1:nrow(subDT), size, replace=T), ]
  if(type=="decr") newDT <- subDT[sample(1:nrow(subDT), size, replace=F), ]
  newDT
}

balance_classes <- function(min.size, max.size, imgTrainDT){
  library(data.table)
  numInClasses <- imgTrainDT[, .N, by=.outcome]
  
  smallClasses <- as.character( numInClasses[N<min.size, .outcome] )
  largeClasses <- as.character( numInClasses[N>max.size, .outcome] )
  
  DTs <- imgTrainDT[.outcome %in% smallClasses, Sampling(.SD, min.size, type="incr"),
             by=.outcome, .SDcols=names(imgTrainDT)]
  setnames(DTs, 1, "to.delete");  DTs[, to.delete:=NULL]
  
  DTl <- imgTrainDT[.outcome %in% largeClasses, Sampling(.SD, max.size, type="decr"),
             by=.outcome, .SDcols=names(imgTrainDT)]
  setnames(DTl, 1, "to.delete");  DTl[, to.delete:=NULL]
    
  DT3 <- imgTrainDT[! .outcome %in% c(smallClasses, largeClasses), ]

  out <- rbindlist(list(DTs, DTl, DT3))
  out
}
