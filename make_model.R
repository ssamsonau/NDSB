# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread(unzip("imgTrainDT.zip"))
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

#descrCor <- cor(imgTrainDT)
#highlyCorDescr <- findCorrelation(descrCor, cutoff = .8)
#imgTrainDT[, eval(highlyCorDescr):=NULL]


### SUBSET data for small computer
subsetSize = -1 
#--------------------------------------------------

imgTrainDT[, output:=
             sapply( strsplit( pathCol, "&"), "[", 1) ]

#imgTrainDT[, genType:=as.factor(
#  sapply( strsplit(as.character(imgTrainDT$output), "_") , "[", 1))]

imgTrainDT[, output:=factor(output)]

library(h2o)
localH2O = h2o.init(nthreads=-1)

#subsetting the data
if(subsetSize != -1){
  set.seed(123)
  subsetVect <- sample(1:nrow(imgTrainDT), subsetSize)
  trainDT.h2o <- as.h2o(client = localH2O, imgTrainDT[subsetVect, ], header=T)  
}else{
  trainDT.h2o <- as.h2o(client = localH2O, imgTrainDT, header=T)
}
print(str(trainDT.h2o[, 1:10]))
Ncols <- ncol(trainDT.h2o)
print(str(trainDT.h2o[, (Ncols-10):Ncols]))

##-------------------------------------------------------
#"train a model for output"
train_hex_split <- h2o.splitFrame(trainDT.h2o, ratios = 0.8, shuffle = TRUE)

grid_search <- h2o.deeplearning(x = c( grep("V", names(trainDT.h2o), value=T)),
                                y = "output",
                                data = train_hex_split[[1]],
                                validation = train_hex_split[[2]],
                                #nfolds = 5,
                                
                                hidden=list(c(40, 40)),
                                epochs = 100,
                                activation=c("Rectifier"),
                                classification = TRUE,
                                balance_classes = FALSE, 
                                adaptive_rate = TRUE,
                                rho = c(0.92, 0.98),
                                epsilon= c(1e-8, 1e-6),
                                #l2=c(1e-5, 1e-4),
                                #l1=c(0, 1e-5),
                                fast_mode=TRUE)


best_model <- grid_search@model[[1]]
best_params <- best_model@model$params

h2o.saveModel(best_model, dir=".", name="best_model.Rdata", force = T)

print(best_model)

#make prediction of output
predicted_output <- h2o.predict(best_model, train_hex_split[[2]])
#predicted_output$predict

#form a submission data table
resultsDT <- data.table(as.matrix(predicted_output))
resultsDT[, predict:=NULL]

source("mcLogLoss.R")
valDT <- data.table( as.matrix(train_hex_split[[2]]) )
mcLogLoss(valDT$output, resultsDT)


#print(best_params)

#print("error on test set")
#predicted <- h2o.predict(best_model, test_hex)
#h2o.confusionMatrix(predicted$predict, test_hex$output)["Totals", "Error"]
