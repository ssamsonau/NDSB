rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 

numOfSlices <- 50  

################################
#count all files
numberOfImages <- 0
for(folderName in folderNames){
  numberOfImages <- numberOfImages + length(dir(paste0(rootDataDir, folderName)))
}

library(jpeg)
# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)
imgTrainDT <- data.table( matrix(0, ncol=1, nrow=numOfSlices*7)  )

library(EBImage)
source("EBimageFeatureExtraction.R")
i <- 1
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- dir(imgDir)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
      
    ImFeatures <- getFeatures(img, numOfSlices=numOfSlices)
    imgTrainDT[ , paste0(folderName, "&", imgName):= ImFeatures] 

    i <- i + 1    
  }
} 
imgTrainDT[, V1:=NULL]
print(object.size(imgTrainDT), units="Mb") 

# transpose before writing to file. 
imgTrainDT <- t(imgTrainDT) 

write.csv(imgTrainDT, file="imgTrainDT.csv")  
system2("C://Program Files/7-Zip/7z.exe", "a -tzip imgTrainDT.zip imgTrainDT.csv")

