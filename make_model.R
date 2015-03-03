t1 <- Sys.time()
# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread("41features.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

imgTrainDT[, .outcome:= sapply( strsplit( pathCol, "&"), "[", 1) ]
imgTrainDT[grep("shrimp-like_other", .outcome), .outcome:="shrimp_like_other"]
imgTrainDT[, .outcome:=factor(.outcome)]

##-------------------------------------------------------
#"train a model for outcome"
library(caret)
set.seed(3456)

#parallel in Windows
#library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
#parallel in Unix
#require('doMC');  registerDoMC()

dir_root = "E:/Temp/forest/"
dir_models=paste0(dir_root, "separate/")

weights_v <- imgTrainDT[, rep(nrow(imgTrainDT)/.N, .N), by=.outcome]$V1

class_frequency <- imgTrainDT[, .N, by=.outcome]$N
sample_size <- class_frequency
sample_size[class_frequency > 5*min(class_frequency)] <- 5*min(class_frequency)-1

###################################### fit one model
fit_one_model <- function(i){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
  fitControl <- trainControl(method = "oob", verboseIter=T #,classProbs=T
  )
  Grid <-  expand.grid(mtry=c(21) )
  #Fit <- train(factor(.outcome) ~ ., data = imgTrainDT_kn[.outcome %in% 
  #                                                 unique(imgTrainDT_kn$.outcome)[1:20]],
  Fit <- train(x=imgTrainDT[, .SD, .SDcols=grep(".outcome", names(imgTrainDT), invert=T)], 
               y=imgTrainDT[, .outcome], 
               data = imgTrainDT, 
               method = "rf", 
               weights = weights_v,   
               ntree=100, 
               trControl = fitControl, 
               norm.votes=FALSE # to combine forests 
               ,metric="Kappa"
               ,strata=imgTrainDT[, .outcome],
               ,sampsize = sample_size   #rep(9, nlevels(imgTrainDT$.outcome) )
                 
               ,tuneGrid=Grid
  )
  save(Fit, file=paste0(dir_models, "rf_fit_",i, ".Rdata") )  
  stopCluster(cl)
}

#################################  Fit several models
fit_models <- function(start_number=1, number_of_models=1){
  i<- start_number; set.seed(i)
  while(i <= number_of_models){
    print(paste0("fitting model N ", i))
    fit_one_model(i)
    i <- i+1  
  }
}

######################## apply 
fit_models(1, 1)

t2 <- Sys.time()
print(t2-t1)

load(file=paste0(dir_models, "rf_fit_",1, ".Rdata") )
sink("log.txt"); 
print(Fit); 
#CM <- confusionMatrix(predict(rf.all, imgTrainDT), imgTrainDT$.outcome)
#print(CM$byClass)
print(t2-t1); 
sink()
