rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 

#count all files
numberOfImages <- 0
for(folderName in folderNames){
  numberOfImages <- numberOfImages + length(grep("r50_", dir(paste0(rootDataDir, folderName))))
}

library(jpeg)
# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)
imgTrainDT <- data.table( matrix(0, ncol=1, nrow=2500)  )

i <- 1
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- grep("r50_", dir(imgDir), value=TRUE)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
    imgTrainDT[ , paste0(folderName, "&", imgName):= as.vector(img)] 
    i <- i + 1    
  }
} 
imgTrainDT[, V1:=NULL]
print(object.size(imgTrainDT), units="Mb") 

# transpose before writing to file. 
imgTrainDT <- t(imgTrainDT)

#save(imgTrainDT, file="imgTrainDT.Rdata")
#save(output, file="output.Rdata")
write.csv(imgTrainDT, file="imgTrainDT.csv")  
