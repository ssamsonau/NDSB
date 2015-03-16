rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 
radial_splits = 10
#size_im <- 30
################################
# Set MKL threads for Revolution R Open
if(require(Revobase)) {library(doParallel); setMKLthreads(detectCores())};
#
library(jpeg)
library(EBImage)
source("EBimageFeatureExtraction.R")
img <- 1- readJPEG("sample.jpg")
number_of_features <- length(getFeatures(img, Splits = radial_splits))

#count all files
numberOfImages <- 0
for(folderName in folderNames){
  numberOfImages <- numberOfImages + length(dir(paste0(rootDataDir, folderName)))
}

# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)
imgTrainDT <- data.table( matrix(0, ncol=1, nrow= number_of_features)  )

t1 <- Sys.time()
i <- 1 
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  i <- ncol(imgTrainDT)
  cat("done with:  ", i-1, "/",  numberOfImages, "  files\n")
  print("estimated time to complete ")
  print(round((Sys.time()-t1)/i * (numberOfImages-i), digits = 2) )
  cat("working with the folder: ", imgDir, "\n")   
  
  imgNames <- dir(imgDir)
  
  #for(imgName in imgNames){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
  imgTrainDT_folder <- foreach(imgName = imgNames, .combine=cbind) %dopar% {
    library(data.table)
    library(jpeg)        
    imgTrainDT_local <- data.table( matrix(0, ncol=1, nrow= number_of_features)  )
    
    img <- readJPEG( paste0(imgDir, imgName) ) 
      
    ImFeatures <- getFeatures(img, Splits = radial_splits)
    imgTrainDT_local[ , paste0(folderName, "&", imgName):= ImFeatures] 
    
    imgTrainDT_local[, V1:=NULL]
    imgTrainDT_local
  }
  stopCluster(cl)
  imgTrainDT <- cbind(imgTrainDT, imgTrainDT_folder)
} 


imgTrainDT[, V1:=NULL]
print(object.size(imgTrainDT), units="Mb") 

# transpose before writing to file. 
imgTrainDT <- t(imgTrainDT) 

write.csv(imgTrainDT, file=paste0(number_of_features, "features.csv")  )
system2("C://Program Files/7-Zip/7z.exe", 
        paste0("a -tzip ", number_of_features, "features.zip ", 
               number_of_features, "features.csv") )

print(paste0("number of features ",  number_of_features) )
