t1 <- Sys.time()
# load prepared data
#-------------------------------------------------
library(data.table)
#imgTrainDT <- fread(unzip("imgTrainDT_128.zip"))
imgTrainDT <- fread("imgTrainDT_128.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

imgTestDT <- fread("imgTestDT_128.csv")
setnames(imgTestDT, 1, "filename")
imgTestDT[, filename:=NULL]

#find variables wich have NA in train or test
col.with.na.train <- names(imgTrainDT)[ sapply(imgTrainDT, anyNA ) | 
                                          sapply(lapply(imgTrainDT, is.infinite), sum)]
col.with.na.test <- names(imgTestDT)[sapply(imgTestDT, anyNA ) | 
                                       sapply(lapply(imgTestDT, is.infinite), sum)]
col.with.na <- unique(c(col.with.na.test, col.with.na.train))
if(length(col.with.na) > 0) imgTrainDT[, eval(col.with.na):=NULL]


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

imgTrainDT[, .outcome:=
             sapply( strsplit( pathCol, "&"), "[", 1) ]

#imgTrainDT[, genType:=as.factor(
#  sapply( strsplit(as.character(imgTrainDT$outcome), "_") , "[", 1))]

imgTrainDT[grep("shrimp-like_other", .outcome), .outcome:="shrimp_like_other"]
imgTrainDT[, .outcome:=factor(.outcome)]

##-------------------------------------------------------
#"train a model for outcome"
set.seed(3456)

#balance classes
numInClasses <- imgTrainDT[, .N, by=.outcome]
summary(numInClasses[, N])
hist(numInClasses[, N])

source("balance_classes.R")
balancedTrainDT <- balance_classes(min.size = 50, max.size = 500, imgTrainDT)
numInClasses <- balancedTrainDT[, .N, by=.outcome]
summary(numInClasses[, N])

source("mcLogLoss_metrics.R")

fitControl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter=T, 
  classProbs=T,
  summaryFunction=mcLogloss_metrics)

#parallel in Windows
library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
#parallel in Unix
#require('doMC');  registerDoMC()

#imgTrainDT[, cl:=as.numeric(imgTrainDT$.outcome)]
#rfFit <- train(factor(.outcome) ~ ., data = balancedTrainDT[1:5000],
rfFit <- train(.outcome ~ ., data = balancedTrainDT,       
               method = "rf",
               
               ntree=50, 
               trControl = fitControl, 
               metric="mcLogloss", 
               maximize=F)

print(rfFit)
#stopCluster(cl)
#system2("C://Windows/System32/cmd.exe", "taskkill /F /IM Rscript.exe")

save(rfFit, file="model_rf.Rdata")


predicted <- predict(rfFit, newdata=imgTrainDT, type="prob")
resultsDT <- data.table(predicted)
source("mcLogLoss.R")
print( mcLogLoss(imgTrainDT$.outcome, resultsDT, ignore.Inf = T))

#print("error on test set")
#predicted <- h2o.predict(best_model, test_hex)
#h2o.confusionMatrix(predicted$predict, test_hex$outcome)["Totals", "Error"]

t2 <- Sys.time()
print(t2-t1)

sink("log.txt")
print(rfFit)
print(t2-t1)
sink()

