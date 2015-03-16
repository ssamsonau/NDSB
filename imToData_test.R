rm(list=ls())
rootDataDir <- "E://Temp/NDSB/test/"
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

numberOfImages <- length(dir(rootDataDir))

# Data Table will be filled by columns - this is significantly faster (vs by rows). 
# Data Talbe is faster than matrix from Matrix package
library(data.table)

imgTestDT <- data.table( matrix(0, ncol=1, nrow= number_of_features)  )

library(EBImage)
source("EBimageFeatureExtraction.R")

imgDir <- rootDataDir
imgNamesDT <- data.table("imgN" = dir(imgDir))
imgNamesDT[, gr:= seq(1:.N) %/% 1000]
max_gr <- imgNamesDT[, max(gr)]

t1 <- Sys.time()

for(grN in 0:max_gr){
  
  print(paste0("done with ", grN, "/", max_gr, " groups" ))
  print("estimated time to complete ")
  print(round((Sys.time()-t1)/grN * (max_gr-grN), digits = 2) )
    
  imgNames <- imgNamesDT[gr == grN, imgN]

  #  for(imgName in imgNames){
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
  imgTestDT_folder <- foreach(imgName = imgNames, .combine=cbind) %dopar% {
    library(data.table)
    library(jpeg)        
    imgTestDT_local <- data.table( matrix(0, ncol=1, nrow= number_of_features)  )
    
    img <- readJPEG( paste0(imgDir, imgName) ) 
    
    ImFeatures <- getFeatures(img, Splits = radial_splits)
    
    imgTestDT_local[ , eval(imgName):= ImFeatures] 
    
    imgTestDT_local[, V1:=NULL]
    imgTestDT_local    
  }
  stopCluster(cl)
  imgTestDT <- cbind(imgTestDT, imgTestDT_folder)
}

 
imgTestDT[, V1:=NULL]
print(object.size(imgTestDT), units="Mb") 

# transpose before writing to file. 
imgTestDT <- t(imgTestDT) 

write.csv(imgTestDT, file=paste0(number_of_features, "featuresTest.csv")  )
system2("C://Program Files/7-Zip/7z.exe", 
        paste0("a -tzip ", number_of_features, "featuresTest.zip ", 
               number_of_features, "featuresTest.csv")) 
