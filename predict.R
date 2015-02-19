# init h2o
library(h2o)
localH2O = h2o.init(nthreads=-1)

#load models
best_model <- h2o.loadModel(localH2O, path="./best_model.Rdata")
best_model_genType <- h2o.loadModel(localH2O, path="best_model_genType.Rdata")

#load test data
library(data.table)
imgTestDT <- fread(unzip("imgTestDT.zip"))
setnames(imgTestDT, 1, "filename")
#load("imgTestDT.Rdata")

#load  test data to h2o
testDT.h2o <- as.h2o(client = localH2O, imgTestDT, header=T)
print(str(testDT.h2o))

#make prediction of genType
predicted_genType <- h2o.predict(best_model_genType, testDT.h2o)
predicted_genType$predict

#add predicted genType as a coloumn to test data
testDT.h2o <- cbind(testDT.h2o, predicted_genType$predict)

#make prediction of output
predicted_output <- h2o.predict(best_model, testDT.h2o)
predicted_output$predict

#form a submission data table
resultsDT <- data.table(as.matrix(predicted_output))
resultsDT[, predict:=NULL]

#when I do subset of data, not all types are presented in prediction. Let us manyally 
#add them to ouptut with prob of 0

submissionVect <- unlist( read.csv("E://Temp/NDSB/sampleSubmission.csv", nrow=1, header=F) )
submissionVect <- sub("-", ".", submissionVect)
missing <- submissionVect[ ! submissionVect %in% names(resultsDT) ]

resultsDT[, eval(missing):=0]

imgNames_original <- sapply(strsplit(imgTestDT$filename, "_"), "[", 2)
resultsDT[, image:=imgNames_original]
new_order <- match(submissionVect, names(resultsDT))
setcolorder(resultsDT, new_order)

write.csv(resultsDT, file="to_be_submitted.csv", quote=F, row.names=F )

system2("C://cygwin64/bin/sed.exe", " -i 's/shrimp.like/shrimp-like/g' to_be_submitted.csv")
