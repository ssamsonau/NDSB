rm(list=ls())
rootDataDir <- "E://Temp/NDSB/test/"
imgDir <- rootDataDir

library(jpeg)
#library('Matrix') 

imgNames <- grep("r50_", dir(imgDir), value=TRUE)
numberOfImages <- length(imgNames)

# matrix will be filled by columns - this is significantly faster (vs by rows). 
#imgMatix_test <- Matrix(0, ncol = numberOfImages, nrow = 2500, sparse = T)
i <- 1

library(data.table)
imgTestDT <- data.table( matrix(0, ncol=0, nrow=2500)  )

for(imgName in imgNames){
  cat("file:  ", i, "/",  numberOfImages, "\n")
  img <- readJPEG( paste0(imgDir, imgName) ) 
  #imgMatix_test[ , i] <- as.vector(img) 
  imgTestDT[ , eval(imgName):= as.vector(img)] 
  #setnames(imgTestDT, paste0("V", i), imgName)
  i <- i + 1
}
imgTestDT[, V1=NULL]
print(object.size(imgTestDT), units="Mb")  

# transpose before writing to file. 
imgTestDT <- t(imgTestDT)
save(imgTestDT, file="imgTestDT.Rdata")
