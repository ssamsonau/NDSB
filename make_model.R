t1 <- Sys.time()
# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread("41features.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

#library(EBImage)
#display(matrix(imgTrainDT[5000, .SD, .SDcols=1:900], ncol=30))

#preprocess with caret
#--------------------------------------------------
library(caret)
#nzv <- nearZeroVar(imgTrainDT)
#imgTrainDT[, eval(nzv):=NULL]

#descrCor <- cor(imgTrainDT)
#highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
#imgTrainDT[, eval(highlyCorDescr):=NULL]

#pca_trans <- preProcess(imgTrainDT, method  = c("center", "scale", "pca"), thresh=0.99)
#imgTrainDT <- data.table( predict(pca_trans, imgTrainDT) )
#--------------------------------------------------

imgTrainDT[, .outcome:=
             sapply( strsplit( pathCol, "&"), "[", 1) ]

imgTrainDT[grep("shrimp-like_other", .outcome), .outcome:="shrimp_like_other"]
imgTrainDT[, .outcome:=factor(.outcome)]

##-------------------------------------------------------
#"train a model for outcome"
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

#Grid <-  expand.grid(mtry = c(2, 4, 8, 16, 32, 64, 128) )
#Grid <- expand.grid(C=c(1e-5, 1e-3, 1e-1, 10, 100))

#imgTrainDT[, cl:=as.numeric(imgTrainDT$.outcome)]
#Fit <- train(factor(.outcome) ~ ., data = imgTrainDT[1000:1200],
Fit <- train(.outcome ~ ., data = imgTrainDT,       
               method = "rf",
               
               ntree=500, 
               trControl = fitControl, 
               metric="Kappa" 
               #,tuneGrid=Grid
               )

print(Fit)
#stopCluster(cl)
#system2("C://Windows/System32/cmd.exe", "taskkill /F /IM Rscript.exe")

save(Fit, file="model_rf_41features.Rdata")

t2 <- Sys.time()
print(t2-t1)

sink("log.txt"); 
print(Fit); 
CM <- confusionMatrix(predict(Fit, imgTrainDT), imgTrainDT$.outcome)
print(CM$byClass)
print(t2-t1); 
sink()
