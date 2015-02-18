load("imgMatix.Rdata")
load("output.Rdata")

library(data.table)
imgDT <- data.table( as.matrix(imgMatix) )
#save(imgDT, file="imgMatix_dense.Rdata")

#library(RSofia)

imgDT[, output:=output]

temp <- strsplit(as.character(imgDT$output), "_")
imgDT[, genType:=as.factor(sapply(temp, "[", 1))]


library(h2o)
localH2O = h2o.init(nthreads=-1)

subsetVect <- sample(1:nrow(imgDT), 1000)

trainDT.h2o <- as.h2o(client = localH2O, imgDT[subsetVect, ], header=T)
print(str(trainDT.h2o))


random <- h2o.runif(trainDT.h2o, seed = 123456789)
train_hex <- h2o.assign(trainDT.h2o[random < .8,], "train_hex")
valid_hex <- h2o.assign(trainDT.h2o[random >= .8 & random < .9,], "valid_hex")
test_hex  <- h2o.assign(trainDT.h2o[random >= .9,], "test_hex")



"train a model for general type"

## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
grid_search_genType <- h2o.deeplearning(x = grep("V", names(trainDT.h2o), value=T),
                                y = "genType",
                                data = trainDT.h2o,
                                validation = valid_hex,
                                #nfolds = 5,
                                hidden=list(c(5, 5)),
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

h2o.saveModel(object=best_model_genType, dir=".", name="best_model_genType.Rdata")

print(best_model_genType)
#print(best_params_genType)

print("error on test set _genType")
predicted_genType <- h2o.predict(best_model_genType, test_hex)
h2o.confusionMatrix(predicted_genType$predict, test_hex$genType)

###------------------------------------------
"train a model for output"

## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
grid_search <- h2o.deeplearning(x = c( grep("V", names(trainDT.h2o), value=T), "genType"),
                                y = "output",
                                data = trainDT.h2o,
                                validation = valid_hex,
                                #nfolds = 5,
                                hidden=list(c(20, 20)),
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


h2o.saveModel(best_model, dir=".", name="best_model.Rdata")

print(best_model)
#print(best_params)

print("error on test set")
predicted <- h2o.predict(best_model, test_hex)
tail( h2o.confusionMatrix(predicted$predict, test_hex$output) )