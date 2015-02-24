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
highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
imgTrainDT[, eval(highlyCorDescr):=NULL]

pca_trans <- preProcess(imgTrainDT, method  = "pca", thresh=0.99)
imgTrainDT <- predict(pca_trans, imgTrainDT)

imgTrainDT <- data.table(imgTrainDT)

### SUBSET data for small computer
subsetSize <- -1 
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

#http://0xdata.com/docs/master/model/deep-learning/
grid_search <- h2o.deeplearning(x = c( grep("PC", names(trainDT.h2o), value=T)),
                                y = "output",
                                data = train_hex_split[[1]], #trainDT.h2o, 
                                validation = train_hex_split[[2]],
                                #nfolds = 4,
                                
                                hidden=list(c(1000, 1000, 1000)),
                                epochs = 100,
                                activation=c("Rectifier"),
                                classification = TRUE,
                                balance_classes = FALSE, 
                                adaptive_rate = TRUE,
                                rho = 0.98, #c(0.92, 0.98),
                                epsilon= 1e-8, #c(1e-8, 1e-6),
                                #l2=c(1e-5, 1e-3, 1e-2, 1),
                                l1=c(0, 1e-5, 1e-3, 1),
                                fast_mode=TRUE)


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
