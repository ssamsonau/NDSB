rm(list=ls())
rootDataDir <- "E://Temp/NDSB/test/"
folderNames <- dir(rootDataDir) 

radial_splits = 10

################################
number_of_features <- 46 +  radial_splits*4 + radial_splits*8

#count all files
numberOfImages <- length(dir(rootDataDir))

library(jpeg)
# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)

imgTestDT <- data.table( matrix(0, ncol=1, nrow= number_of_features)  )

library(EBImage)
source("EBimageFeatureExtraction.R")
#source("EBimageTurnImage.R")

i <- 1 
  imgDir <- rootDataDir
  imgNames <- dir(imgDir)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
      
    ImFeatures <- getFeatures(img, Splits = radial_splits)
    #img_r <- turnImage(img = img, sizeIm = sizeIm)

    #imgTestDT[ , paste0(folderName, "&", imgName):= c(ImFeatures, img_r)] 
    imgTestDT[ , eval(imgName):= ImFeatures] 
    
    i <- i + 1    
  }
 
imgTestDT[, V1:=NULL]
print(object.size(imgTestDT), units="Mb") 

# transpose before writing to file. 
imgTestDT <- t(imgTestDT) 

write.csv(imgTestDT, file=paste0(number_of_features, "featuresTest.csv")  )
system2("C://Program Files/7-Zip/7z.exe", 
        paste0("a -tzip ", number_of_features, "featuresTest.zip ", 
               number_of_features, "featuresTest.csv"))


