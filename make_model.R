# load prepared data
library(data.table)
imgTrainDT <- fread(unzip("imgTrainDT.zip"))
#imgTrainDT <- fread("imgFeaturesTrainDT.csv")
setnames(imgTrainDT, 1, "path")
#load("imgTrainDT.Rdata")
#load("output.Rdata")

#scale.f <-  function(dt){ as.vector(scale(dt)) }
imgTrainDT[, names(imgTrainDT)[2:8]:= lapply(.SD, scale), .SDcols = 2:8 ]


### SUBSET data for small computer
subsetSize = -1 
#-------------------------

imgTrainDT[, output:=
             sapply( strsplit( imgTrainDT$path, "&"), "[", 1) ]

imgTrainDT[, genType:=as.factor(
  sapply( strsplit(as.character(imgTrainDT$output), "_") , "[", 1))]

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

#takes too much time and uses only one core
#random <- h2o.runif(trainDT.h2o, seed = 123456789)
#train_hex <- h2o.assign(trainDT.h2o[random < .8,], "train_hex")
#valid_hex <- h2o.assign(trainDT.h2o[random >= .8 & random < .9,], "valid_hex")
#test_hex  <- h2o.assign(trainDT.h2o[random >= .9,], "test_hex")

train_hex_split <- h2o.splitFrame(trainDT.h2o, ratios = 0.8, shuffle = TRUE)

"train a model for general type"

## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
grid_search_genType <- h2o.deeplearning(x = grep("V", names(trainDT.h2o), value=T),
                                y = "genType",
                                data = train_hex_split[[1]],
                                validation = train_hex_split[[2]],
                                #nfolds = 5,
                                hidden=list(c(20, 20, 20)),
                                epochs = 60,
                                activation=c("Tanh", "Rectifier"),
                                classification = TRUE,
                                balance_classes = FALSE, 
                                adaptive_rate = TRUE,
                                epsilon= c(1e-4, 1e-6, 1e-8, 1e-10),
                                rho = c(0.9, 0.95, 0.99),
                                #l2=c(1e-5, 1e-4),
                                #l1=c(0, 1e-5),
                                fast_mode=TRUE
)

best_model_genType <- grid_search_genType@model[[1]]
best_params_genType <- best_model_genType@model$params

h2o.saveModel(object=best_model_genType, dir=".", name="best_model_genType.Rdata", force=T)

print(best_model_genType)
#print(best_params_genType)

#print("error on test set _genType")
#predicted_genType <- h2o.predict(best_model_genType, test_hex)
#h2o.confusionMatrix(predicted_genType$predict, test_hex$genType)["Totals", "Error"]

###------------------------------------------
"train a model for output"

## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
grid_search <- h2o.deeplearning(x = c( grep("V", names(trainDT.h2o), value=T), "genType"),
                                y = "output",
                                data = train_hex_split[[1]],
                                validation = train_hex_split[[2]],
                                #nfolds = 5,
                                hidden=list(c(20, 20, 20)),
                                epochs = 60,
                                activation=c("Tanh", "Rectifier"),
                                classification = TRUE,
                                balance_classes = FALSE, 
                                adaptive_rate = TRUE,
                                epsilon= c(1e-4, 1e-6, 1e-8, 1e-10),
                                rho = c(0.9, 0.95, 0.99),
                                #l2=c(1e-5, 1e-4),
                                #l1=c(0, 1e-5),
                                fast_mode=TRUE
)


best_model <- grid_search@model[[1]]
best_params <- best_model@model$params


h2o.saveModel(best_model, dir=".", name="best_model.Rdata", force = T)

print(best_model)
#print(best_params)

#print("error on test set")
#predicted <- h2o.predict(best_model, test_hex)
#h2o.confusionMatrix(predicted$predict, test_hex$output)["Totals", "Error"]
