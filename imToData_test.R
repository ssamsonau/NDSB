rm(list=ls())
rootDataDir <- "E://Temp/NDSB/test/"
folderNames <- dir(rootDataDir) 

#numOfSlices <- 20 
#sizeIm <- 30

################################
#count all files
numberOfImages <- length(dir(rootDataDir))

library(jpeg)
# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)

imgTestDT <- data.table( matrix(0, ncol=1, nrow=94)  )

library(EBImage)
source("EBimageFeatureExtraction.R")
source("EBimageTurnImage.R")

i <- 1 
  imgDir <- rootDataDir
  imgNames <- dir(imgDir)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
      
    ImFeatures <- getFeatures(img)
    
    #img_r <- turnImage(img = img, sizeIm = sizeIm)
    
    #imgTestDT[ , paste0(folderName, "&", imgName):= c(ImFeatures, img_r)] 
    imgTestDT[ , eval(imgName):= ImFeatures] 
    
    
    # save images for diagnostics
    #diag.path <- paste0("./diagnostics/", folderName)
    #if(! file.exists(diag.path)){dir.create(diag.path)} 
    #writeJPEG(ifelse(img_r, 1, 0), target=paste0(diag.path, "/", imgName), bg="black")
    # end diagnostics
        
    i <- i + 1
    
  }
 
imgTestDT[, V1:=NULL]
print(object.size(imgTestDT), units="Mb") 

# transpose before writing to file. 
imgTestDT <- t(imgTestDT) 

write.csv(imgTestDT, file="imgTestDT.csv")  
system2("C://Program Files/7-Zip/7z.exe", "a -tzip imgTestDT.zip imgTestDT.csv")

