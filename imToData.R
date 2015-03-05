rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 
#radial_splits = 10
size_im <- 30
################################
#number_of_features <- 46 +  radial_splits*4 + radial_splits*8
number_of_features <- size_im^2

#count all files
numberOfImages <- 0
for(folderName in folderNames){
  numberOfImages <- numberOfImages + length(dir(paste0(rootDataDir, folderName)))
}

library(jpeg)
# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)
imgTrainDT <- data.table( matrix(0, ncol=1, nrow= number_of_features)  )

library(EBImage)
#source("EBimageFeatureExtraction.R")
source("EBimageTurnImage.R")

i <- 1 
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- dir(imgDir)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
      
    #ImFeatures <- getFeatures(img, Splits = radial_splits)
    
    img_r <- turnImage(img = img, size_im = size_im)
    
    #imgTrainDT[ , paste0(folderName, "&", imgName):= c(c(img_r), ImFeatures)] 
    #imgTrainDT[ , paste0(folderName, "&", imgName):= ImFeatures] 
    imgTrainDT[ , paste0(folderName, "&", imgName):= c(img_r)] 
    
    i <- i + 1
    
  }
} 
imgTrainDT[, V1:=NULL]
print(object.size(imgTrainDT), units="Mb") 

# transpose before writing to file. 
imgTrainDT <- t(imgTrainDT) 

write.csv(imgTrainDT, file=paste0(number_of_features, "features30x30binaryLargest.csv")  )
system2("C://Program Files/7-Zip/7z.exe", 
        paste0("a -tzip ", number_of_features, "features30x30binaryLargest.zip ", 
               number_of_features, "features30x30binaryLargest.csv") )

print(paste0("number of features ",  number_of_features) )
