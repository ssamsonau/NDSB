
#load models
load("model_rf_41features_distort.Rdata")
print(Fit)

#load train And test data
library(data.table)
imgTrainDT <- fread("41features.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

imgTestDT <- fread("41featuresTest.csv")
setnames(imgTestDT, 1, "filename")
fileNameCol <- imgTestDT$filename
imgTestDT[, filename:=NULL]


#preprocess with caret
#--------------------------------------------------
library(caret)
#c_s_trans <- preProcess(imgTrainDT, method  = c("center", "scale"))
#imgTestDT <- data.table( predict(c_s_trans, imgTestDT) )

#make prediction of output
#--------------
predicted <- predict(Fit, newdata=imgTestDT, type="prob")

#form a submission data table
resultsDT <- data.table(predicted)

#when I do subset of data, not all types are presented in prediction. Let us manyally 
#add them to ouptut with prob of 0

submissionVect <- unlist( read.csv("E://Temp/NDSB/sampleSubmission.csv", nrow=1, header=F) )
submissionVect <- sub("-", "_", submissionVect)
missing <- submissionVect[ ! submissionVect %in% names(resultsDT) ]

resultsDT[, eval(missing):=0]

resultsDT[, image:=fileNameCol]
new_order <- match(submissionVect, names(resultsDT))
setcolorder(resultsDT, new_order)

write.csv(resultsDT, file="to_be_submitted.csv", quote=F, row.names=F )

system2("C://cygwin64/bin/sed.exe", " -i 's/shrimp_like/shrimp-like/g' to_be_submitted.csv")

system2("C://Program Files/7-Zip/7z.exe", "a -tzip to_be_submitted.zip to_be_submitted.csv")
