t1 <- Sys.time()
# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread("41features_distort.csv")
setnames(imgTrainDT, 1, "path")

pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

#preprocess with caret
library(caret)
#c_s_trans <- preProcess(imgTrainDT, method  = c("center", "scale"))
#imgTrainDT <- data.table( predict(c_s_trans, imgTrainDT) )

imgTrainDT[, .outcome:= sapply( strsplit( pathCol, "&"), "[", 1) ]
imgTrainDT[grep("shrimp-like_other", .outcome), .outcome:="shrimp_like_other"]

#imgTrainDT_kn <- imgTrainDT[ grep("unknown", pathCol, invert = T), ]
#imgTrainDT_un <- imgTrainDT[ grep("unknown", pathCol), ]

#source("balance_classes.R")
#hist(imgTrainDT[, .N, by=.outcome]$N)
#imgTrainDT <- balance_classes(min.size = 500, max.size = 1000, imgTrainDT = imgTrainDT)


#imgTrainDT_kn[, .outcome:=factor(.outcome)]
#imgTrainDT_un[, .outcome:=factor(.outcome)]

imgTrainDT[, .outcome:=factor(.outcome)]

##-------------------------------------------------------
#"train a model for outcome"
library(caret)
set.seed(3456)

source("mcLogLoss_metrics.R")

fitControl <- trainControl(
  method = "cv",
  number = 4,
  verboseIter=T 
  #,classProbs=T
  )

#parallel in Windows
library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
#parallel in Unix
#require('doMC');  registerDoMC()

#Grid <-  expand.grid(mtry=c(2, 21, 41),  maxnodes=c(5, 10, 20) )
#Grid <- expand.grid(C=c(1e-1, 1, 10, 100, 1000))
#Grid <- expand.grid(C=c(1e-2, 1e-1, 1, 10, 100))

#imgTrainDT[, cl:=as.numeric(imgTrainDT$.outcome)]
#Fit <- train(factor(.outcome) ~ ., data = imgTrainDT_kn[.outcome %in% 
#                                                          unique(imgTrainDT_kn$.outcome)[1:20]],
Fit <- train(.outcome ~ ., data = imgTrainDT,       
               method = "rf",
               
               ntree=500,
               trControl = fitControl 
               ,metric="Kappa" 
               #,tuneGrid=Grid
               )

print(Fit)
#stopCluster(cl)
#system2("C://Windows/System32/cmd.exe", "taskkill /F /IM Rscript.exe")

save(Fit, file="model_rf_41features_distort.Rdata")

t2 <- Sys.time()
print(t2-t1)

sink("log.txt"); 
print(Fit); 
CM <- confusionMatrix(predict(Fit, imgTrainDT), imgTrainDT$.outcome)
print(CM$byClass)
print(t2-t1); 
sink()
