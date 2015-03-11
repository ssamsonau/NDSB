#load train And test data
library(data.table)

imgTrainDT <- fread("316features.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

library(caret)
nzv <- nearZeroVar(imgTrainDT)
imgTrainDT[, eval(nzv):=NULL]

###infinite to NA and the impute
imgTrainDT[, names(imgTrainDT):=lapply(.SD, function(x){replace(x, is.infinite(x), NA)}), 
           .SDcols=1:ncol(imgTrainDT)]
knn_imp <-  preProcess(imgTrainDT, method = "knnImpute", k=5)
imgTrainDT <- data.table(predict(knn_imp, imgTrainDT))

imgTrainDT[, .outcome:=sapply(strsplit(pathCol, "&"), "[", 1)]
imgTrainDT[, .filename:=sapply(strsplit(pathCol, "&"), "[", 2)]

write.csv(imgTrainDT, file=paste0(316, "features_imp.csv"), row.names = F )


################   test
imgTestDT <- fread("316featuresTest.csv")
setnames(imgTestDT, 1, "filename")
fileNameCol <- imgTestDT$filename
imgTestDT[, filename:=NULL]

#nzv <- nearZeroVar(imgTrainDT)
imgTestDT[, eval(nzv):=NULL]

imgTestDT[, names(imgTestDT):=lapply(.SD, function(x){replace(x, is.infinite(x), NA)}), 
          .SDcols=1:ncol(imgTestDT)]
knn_imp <-  preProcess(imgTestDT, method = "knnImpute", k=5)
imgTestDT <- data.table(predict(knn_imp, imgTestDT))

imgTestDT[, .filename:=fileNameCol]
write.csv(imgTestDT, file=paste0(316, "featuresTest_imp.csv"), row.names = F  )
