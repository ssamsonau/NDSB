rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 

#numOfSlices <- 20 
#sizeIm <- 30

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

#imgTrainDT <- data.table( matrix(0, ncol=1, nrow=numOfSlices*7+sizeIm^2)  )
imgTrainDT <- data.table( matrix(0, ncol=1, nrow=128)  )


library(EBImage)
source("EBimageFeatureExtraction.R")
#source("EBimageTurnImage.R")

i <- 1 
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- dir(imgDir)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
      
    ImFeatures <- getFeatures(img)
    
    #img_r <- turnImage(img = img, sizeIm = sizeIm)
    
    #imgTrainDT[ , paste0(folderName, "&", imgName):= c(ImFeatures, img_r)] 
    imgTrainDT[ , paste0(folderName, "&", imgName):= ImFeatures] 
    
    
    # save images for diagnostics
    #diag.path <- paste0("./diagnostics/", folderName)
    #if(! file.exists(diag.path)){dir.create(diag.path)} 
    #writeJPEG(ifelse(img_r, 1, 0), target=paste0(diag.path, "/", imgName), bg="black")
    # end diagnostics
        
    i <- i + 1
    
  }
} 
imgTrainDT[, V1:=NULL]
print(object.size(imgTrainDT), units="Mb") 

# transpose before writing to file. 
imgTrainDT <- t(imgTrainDT) 

write.csv(imgTrainDT, file="imgTrainDT_128.csv")  
system2("C://Program Files/7-Zip/7z.exe", "a -tzip imgTrainDT_128.zip imgTrainDT_128.csv")

