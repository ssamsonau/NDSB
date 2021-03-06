t1 <- Sys.time()
# load prepared data
# Set MKL threads for Revolution R Open
if(require(Revobase)) {library(doParallel); setMKLthreads(detectCores())};
#-------------------------------------------------
library(data.table)
dir_root = "E:/Temp/forest/"
imgTrainDT <- fread(paste0(dir_root, "data/316features_imp_joined_V.csv"))

outCol <- imgTrainDT$.outcome
imgTrainDT[, .outcome:=NULL]
imgTrainDT[, .filename:=NULL]

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

for_weight <- imgTrainDT[, nrow(imgTrainDT)/.N, by=.outcome]
setkey(for_weight, .outcome)
rf_weights_v <- for_weight[, V1]
names(rf_weights_v) <- for_weight[, .outcome]

###################################### fit one model  caret
fit_one_model_caret <- function(i){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
  
  fitControl <- trainControl(method = "oob", verboseIter=T 
                             #,classProbs=T, summaryFunction = mcLogloss_metrics  
                             )
  Fit <- train(x=imgTrainDT[, .SD, .SDcols=grep(".outcome", names(imgTrainDT), invert=T)], 
               y=imgTrainDT[, .outcome], 
               data = imgTrainDT, method = "rf", 
               weights = weights_v,   
               ntree=n_trees, 
               norm.votes=FALSE # to combine forests 
               ,metric="Kappa"
               ,tuneLength=detectCores()
               ,trControl = fitControl
  )
  stopCluster(cl)
  save(Fit, file=paste0(dir_models, "rf_fit_caret_",i, ".Rdata") ) 
  print(Fit)
  return(Fit$bestTune$mtry)
}

###################################### fit one model rf
fit_one_model_rf <- function(i){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl); 
  library(foreach); library(randomForest)
  x <- imgTrainDT[, .SD, .SDcols=grep(".outcome", names(imgTrainDT), invert=T)]
  y <- imgTrainDT[, .outcome]
  b_mtry <- b_mtry
  rf_weights_v <- rf_weights_v
  
  Fit <- foreach(ntree=rep(n_trees/detectCores(), detectCores()), .combine=combine, 
                 .packages='randomForest') %dopar% {
                  randomForest(x=x, y=y, ntree=ntree, mtry=b_mtry, 
                               classwt = rf_weights_v)                    
                }
  
  stopCluster(cl)
  save(Fit, file=paste0(dir_models, "rf_fit_rf_",i, ".Rdata") )  
}

#################################  Fit several models
fit_models <- function(start_number=1, number_of_models=1){
  i<- start_number
  while(i <= number_of_models){
    print(paste0("fitting model N ", i))  
    fit_one_model_rf(i)  
    i <- i+1  
  }
}

######################## apply 
#number of trees per file
n_trees = 100 # should be divisible by number of cpu for efficient foreach
#to find best mtry using out of bag error
b_mtry <- fit_one_model_caret(1)
#b_mtry = 166

#to train many models
fit_models(2, 100)

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
 