t1 <- Sys.time()
# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread(unzip("imgTrainDT_128.zip"))
#imgTrainDT <- fread("imgFeaturesTrainDT.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

#preprocess with caret
#--------------------------------------------------
library(caret)
nzv <- nearZeroVar(imgTrainDT)
imgTrainDT[, eval(nzv):=NULL]

col.to.scale <- names(imgTrainDT)
preProcValues <- preProcess(imgTrainDT[, .SD, .SDcols = col.to.scale ], 
                            method = c("center", "scale"))
imgTrainDT[, eval(col.to.scale):=predict(preProcValues, imgTrainDT[, .SD, .SDcols=col.to.scale]) ]


descrCor <- cor(imgTrainDT)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
imgTrainDT[, eval(highlyCorDescr):=NULL]

#pca_trans <- preProcess(imgTrainDT, method  = "pca", thresh=0.99)
#imgTrainDT <- predict(pca_trans, imgTrainDT)

#--------------------------------------------------

imgTrainDT[, output:=
             sapply( strsplit( pathCol, "&"), "[", 1) ]

#imgTrainDT[, genType:=as.factor(
#  sapply( strsplit(as.character(imgTrainDT$output), "_") , "[", 1))]

imgTrainDT[, output:=factor(output)]

##-------------------------------------------------------
#"train a model for output"
set.seed(3456)

fitControl <- trainControl(
  method = "cv",
  number = 5, 
  verboseIter=T, 
  classProbs=T, 
  summaryFunction="Kappa")

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

svmLinearFit <- train(output ~ ., data = imgTrainDT,
                 method = "svmLinear",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
stopCluster(cl)
#system2("C://Windows/System32/cmd.exe", "taskkill /F /IM Rscript.exe")

svmLinearFit



best_model <- grid_search@model[[1]]
best_params <- best_model@model$params

h2o.saveModel(best_model, dir=".", name="best_model.Rdata", force = T)

print(best_model)

print(best_params)


#make prediction of output
#predicted_output <- h2o.predict(best_model, train_hex_split[[2]])
predicted_output <- h2o.predict(best_model, trainDT.h2o)

#form a submission data table
resultsDT <- data.table(as.matrix(predicted_output))
#resultsDT[, predict:=NULL]

source("mcLogLoss.R")
#valDT <- data.table( as.matrix(train_hex_split[[2]]) )
valDT <- data.table( as.matrix(trainDT.h2o) )

print( mcLogLoss(valDT$output, resultsDT))

#print("error on test set")
#predicted <- h2o.predict(best_model, test_hex)
#h2o.confusionMatrix(predicted$predict, test_hex$output)["Totals", "Error"]

t2 <- Sys.time()
print(t2-t1)

sink("out.txt")
print(best_model)
print(best_params)
print(t2-t1)
sink()

