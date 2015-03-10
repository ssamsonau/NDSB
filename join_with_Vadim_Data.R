library(data.table)
indexTr <- unlist(c(fread("From_Vadim/trainIndex.csv")))
indexTr <- indexTr[1:(length(indexTr)-1)] # the last is just coma...

vadTrainDT <- fread("From_Vadim/TrainValues.csv")
setnames(vadTrainDT, 1:ncol(vadTrainDT), paste0("a", names(vadTrainDT)))

vadTrainDT[, n:=indexTr]

#local data
local_data <- fread("316features.csv")
setnames(local_data, 1, "path")
local_path <- local_data$path
local_names <- sapply(strsplit(local_path, "&"), "[", 2)

imgTrainDT[, n:= local_names]

#join
imgTrainDT <- merge(imgTrainDT, vadTrainDT, by="n")
