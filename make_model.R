t1 <- Sys.time()
# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread("img_turn_30_14_features.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

library(EBImage)
display(matrix(imgTrainDT[5000, .SD, .SDcols=1:900], ncol=30))

#preprocess with caret
#--------------------------------------------------
library(caret)
nzv <- nearZeroVar(imgTrainDT)
imgTrainDT[, eval(nzv):=NULL]

descrCor <- cor(imgTrainDT)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
imgTrainDT[, eval(highlyCorDescr):=NULL]

#pca_trans <- preProcess(imgTrainDT, method  = "pca", thresh=0.99)
#imgTrainDT <- predict(pca_trans, imgTrainDT)
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
  number = 5,
  verboseIter=T, 
  classProbs=T)

#parallel in Windows
library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
#parallel in Unix
#require('doMC');  registerDoMC()

#rfGrid <-  expand.grid(mtry = c(2, 4, 8, 16, 32, 64, 128) )

#imgTrainDT[, cl:=as.numeric(imgTrainDT$.outcome)]
#rfFit <- train(factor(.outcome) ~ ., data = balancedTrainDT[1:5000],
sink("iter.txt")
rfFit <- train(.outcome ~ ., data = imgTrainDT,       
               method = "rf",
               
               ntree=50, 
               trControl = fitControl, 
               metric="Kappa" 
               #tuneGrid=rfGrid
               )
sink()

print(rfFit)
#stopCluster(cl)
#system2("C://Windows/System32/cmd.exe", "taskkill /F /IM Rscript.exe")

save(rfFit, file="model_rf_turn_30_14f.Rdata")

t2 <- Sys.time()
print(t2-t1)
sink("log.txt"); print(rfFit); print(t2-t1); sink()
