library(data.table)
#read Vadim's data
indexTr <- unlist(c(fread("E:/Temp/forest/From_Vadim/trainIndex.csv")))
indexTr <- indexTr[1:(length(indexTr)-1)] # the last is just coma...

vadTrainDT <- fread("E:/Temp/forest/From_Vadim/TrainValues.csv")
setnames(vadTrainDT, 1:ncol(vadTrainDT), paste0("a", names(vadTrainDT)))

vadTrainDT[, .filename:=indexTr]

#read local data
local_data <- fread("316features_imp.csv")

#join
imgTrainDT_joined <- merge(local_data, vadTrainDT, by=".filename")

write.csv(imgTrainDT_joined, file=paste0(316, "features_imp_joined_V.csv"), row.names = F )


##########################test data

#read Vadim's data
indexTs <- unlist(c(fread("E:/Temp/forest/From_Vadim/testIndex.csv")))
indexTs <- indexTs[1:(length(indexTs)-1)] # the last is just coma...

vadTestDT <- fread("E:/Temp/forest/From_Vadim/TestValues.csv")
setnames(vadTestDT, 1:ncol(vadTestDT), paste0("a", names(vadTestDT)))

vadTestDT[, .filename:=indexTs]

#read local data
local_data_test <- fread("316featuresTest_imp.csv")

#join
imgTestDT_joined <- merge(local_data_test, vadTestDT, by=".filename")

write.csv(imgTestDT_joined, file=paste0(316, "featuresTest_imp_joined_V.csv"), row.names = F )
