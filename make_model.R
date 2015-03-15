t1 <- Sys.time()
# load prepared data
# Set MKL threads for Revolution R Open
if(require(Revobase)) {library(doParallel); setMKLthreads(detectCores())};
#-------------------------------------------------
library(data.table)
dir_root = "E:/Temp/forest/"
#imgTrainDT <- fread(paste0(dir_root, "data/316features_imp.csv"))
imgTrainDT <- fread(paste0(dir_root, "data/316features_imp_joined_V.csv"))


#imgTrainDTturn <- fread("turned")
#setnames(imgTrainDTturn, 1, "path")
#setnames(imgTrainDTturn, 2:ncol(imgTrainDTturn), 
#         paste0("t", names(imgTrainDTturn)[2:ncol(imgTrainDTturn)])   )
#imgTrainDT <- merge(imgTrainDT, imgTrainDTturn, by="path")
outCol <- imgTrainDT$.outcome
imgTrainDT[, .outcome:=NULL]
imgTrainDT[, .filename:=NULL]

#imgTrainDT <- imgTrainDT[sample(1:nrow(imgTrainDT), 1000), ]

library(caret)
nzv <- nearZeroVar(imgTrainDT)
imgTrainDT[, eval(nzv):=NULL]

#### remove correlated
cor_mat <- cor(imgTrainDT)
highly_cor <- findCorrelation(cor_mat, cutoff = .95)
imgTrainDT[,eval(highly_cor):=NULL]

#####PCA
#prep_pca <-  preProcess(imgTrainDT, method = "pca", t=0.99)
#imgTrainDT <- data.table(predict(prep_pca, imgTrainDT))

#prep_range <-  preProcess(imgTrainDT, method = "range")
#imgTrainDT <- data.table(predict(prep_range, imgTrainDT))
#save(prep_range, file="prep_range.Rdata")

###form outcome coloumn
imgTrainDT[, .outcome:= outCol ]
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

#source("mcLogLoss_metrics.R")

dir_models=paste0(dir_root, "separate/")

weights_v <- imgTrainDT[, rep(nrow(imgTrainDT)/.N, .N), by=.outcome]$V1

#class_frequency <- imgTrainDT[, .N, by=.outcome]$N
#sample_size <- class_frequency
#sample_size[class_frequency > 10*min(class_frequency)] <- 10*min(class_frequency) -1

###################################### fit one model  caret
fit_one_model_caret <- function(i){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
  #library(doParallel);  cl <- makeCluster(1);  registerDoParallel(cl)
  
  fitControl <- trainControl(method = "oob", verboseIter=T 
                             #,classProbs=T, summaryFunction = mcLogloss_metrics
  )
  #Fit <- train(factor(.outcome) ~ ., data = imgTrainDT_kn[.outcome %in% 
  #                                                 unique(imgTrainDT_kn$.outcome)[1:20]],
  Fit <- train(x=imgTrainDT[, .SD, .SDcols=grep(".outcome", names(imgTrainDT), invert=T)], 
               y=imgTrainDT[, .outcome], 
               data = imgTrainDT, method = "rf", 
               weights = weights_v,   
               ntree=n_trees, 
               norm.votes=FALSE # to combine forests 
               ,metric="Kappa"
               #,strata=imgTrainDT[, .outcome],
               #,sampsize = sample_size   #rep(9, nlevels(imgTrainDT$.outcome) )
                 
               ,tuneLength=detectCores()
               #,tuneGrid=expand.grid(mtry=c(111) )
               ,trControl = fitControl
  )
  stopCluster(cl)
  save(Fit, file=paste0(dir_models, "rf_fit_caret_",i, ".Rdata") ) 
  print(Fit)
  return(Fit)
}

###################################### fit one model rf
fit_one_model_rf <- function(i, rf_class_wt){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl); 
  library(foreach); library(randomForest)
  x <- imgTrainDT[, .SD, .SDcols=grep(".outcome", names(imgTrainDT), invert=T)]
  y <- imgTrainDT[, .outcome]
  b_mtry <- b_mtry
  
  Fit <- foreach(ntree=rep(n_trees/detectCores(), detectCores()), .combine=combine, 
                 .packages='randomForest') %dopar% {
                  randomForest(x=x, y=y, ntree=ntree, mtry=b_mtry, 
                              classwt=rf_class_wt)                    
                }
  
  stopCluster(cl)
  #print(Fit$ntree)
  save(Fit, file=paste0(dir_models, "rf_fit_rf_",i, ".Rdata") )

  imgTrainDT[, pred:=Fit$predicted]
  error_rate <- imgTrainDT[, sum(.outcome!=pred)/.N, by=.outcome]
  setkey(error_rate, .outcome)
  imgTrainDT[, pred:=NULL]
  
  rf_class_wt <- error_rate[, V1]
  names(rf_class_wt) <- error_rate[, .outcome]
  #weights_v <- rep(error_rate, class_frequency)
  #rf_class_wt <- error_rate
  rf_class_wt
}

#################################  Fit several models
fit_models <- function(start_number=1, number_of_models=1){
  i<- start_number
  while(i <= number_of_models){
    print(paste0("fitting model N ", i))  
    temp_rf_class_wt <<- fit_one_model_rf(i, rf_class_wt)  
    print(paste0("change in rf_class_wt: ", 
                 sqrt( sum( (temp_rf_class_wt - rf_class_wt)^2) ) ))
    rf_class_wt <<- temp_rf_class_wt
    i <- i+1  
  }
}


######################## apply 
#number of trees per file
n_trees = 100 # should be divisible by number of cpu for efficient foreach
#to find best mtry using out of bag error
Fit <- fit_one_model_caret(1)
b_mtry <- Fit$bestTune$mtry
#b_mtry = 166

Fit <- Fit$finalModel
imgTrainDT[, pred:=Fit$predicted]
error_rate <- imgTrainDT[, sum(.outcome!=pred)/.N, by=.outcome]
setkey(error_rate, .outcome)
imgTrainDT[, pred:=NULL]
rf_class_wt <- error_rate[, V1]
names(rf_class_wt) <- error_rate[, .outcome]
#rf_class_wt <- rep(1, 121)

#################################to train many models
fit_models(2, 100)
save(rf_class_wt, file="rf_class_wt.Rdata")

t2 <- Sys.time()
print(t2-t1)

load(file=paste0(dir_models, "rf_fit_caret_",1, ".Rdata") )
sink("log.txt"); 
print(Fit); 
png(filename="m_plot.png")
library(randomForest); plot(Fit$finalModel)
dev.off()
#CM <- confusionMatrix(predict(rf.all, imgTrainDT), imgTrainDT$.outcome)
#print(CM$byClass)
print(t2-t1); 
sink()
