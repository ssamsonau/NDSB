# init h2o
library(h2o)
localH2O = h2o.init(nthreads=-1)

#load models
best_model <- h2o.loadModel(localH2O, path="./best_model.Rdata")

#load train And test data
library(data.table)
imgTrainDT <- fread(unzip("imgTrainDT_128.zip"))
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]


imgTestDT <- fread(unzip("imgTestDT_128.zip"))
setnames(imgTestDT, 1, "filename")
fileNameCol <- imgTestDT$filename
imgTestDT[, filename:=NULL]

#preprocess with caret
#--------------------------------------------------
library(caret)
nzv <- nearZeroVar(imgTrainDT)
imgTrainDT[, eval(nzv):=NULL]
imgTestDT[, eval(nzv):=NULL]

col.to.scale <- names(imgTrainDT)
preProcValues <- preProcess(imgTrainDT[, .SD, .SDcols = col.to.scale ], 
                            method = c("center", "scale"))
imgTrainDT[, eval(col.to.scale):=predict(preProcValues, imgTrainDT[, .SD, .SDcols=col.to.scale]) ]
imgTestDT[, eval(col.to.scale):=predict(preProcValues, imgTestDT[, .SD, .SDcols=col.to.scale]) ]


descrCor <- cor(imgTrainDT)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
imgTrainDT[, eval(highlyCorDescr):=NULL]
imgTestDT[, eval(highlyCorDescr):=NULL]

pca_trans <- preProcess(imgTrainDT, method  = "pca", thresh=0.95)
imgTestDT <- predict(pca_trans, imgTestDT)

imgTestDT <- data.table(imgTestDT)



#load  test data to h2o
testDT.h2o <- as.h2o(client = localH2O, imgTestDT, header=T)
print(str(testDT.h2o))

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

resultsDT[, image:=fileNameCol]
new_order <- match(submissionVect, names(resultsDT))
setcolorder(resultsDT, new_order)

write.csv(resultsDT, file="to_be_submitted.csv", quote=F, row.names=F )

system2("C://cygwin64/bin/sed.exe", " -i 's/shrimp.like/shrimp-like/g' to_be_submitted.csv")

system2("C://Program Files/7-Zip/7z.exe", "a -tzip to_be_submitted.zip to_be_submitted.csv")
