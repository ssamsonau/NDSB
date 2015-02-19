rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 
#folderNames <- grep("unknown", folderNames, invert=TRUE, value=TRUE)

typeNames <- c()
for(folderName in folderNames){
  dirContent <- dir(paste0(rootDataDir, folderName))
  typeNames <- c(typeNames, rep(folderName, length(dirContent)))
}
output <- factor(typeNames)
numberOfImages <- length(typeNames)

library(jpeg)
#library('Matrix') 

library(data.table)
imgTrainDT <- data.table( matrix(0, ncol=1, nrow=2500)  )
#imgMatix <- Matrix(0, nrow = length(typeNames), ncol = 2500, sparse = TRUE)
#ImgMatix <- matrix(0, nrow = length(typeNames), ncol = 2500)
i <- 1

for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- grep("r50_", dir(imgDir), value=TRUE)
  for(imgName in imgNames){
    cat("file:  ", i, "/",  numberOfImages, "\n")
    img <- readJPEG( paste0(imgDir, imgName) ) 
    #imgMatix_test[ , i] <- as.vector(img) 
    imgTrainDT[ , eval(folderName):= as.vector(img)] 
    #setnames(imgTrainDT, paste0("V", i), folderName)
    i <- i + 1    
  }
} 
imgTrainDT[, V1:=NULL]
print(object.size(imgTrainDT), units="Mb") 

# transpose before writing to file. 
imgTrainDT <- t(imgTrainDT)

save(imgTrainDT, file="imgTrainDT.Rdata")
#save(output, file="output.Rdata")
  