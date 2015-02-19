rm(list=ls())
rootDataDir <- "E://Temp/NDSB/test/"
imgDir <- rootDataDir

library(jpeg)
imgNames <- grep("r50_", dir(imgDir), value=TRUE)
numberOfImages <- length(imgNames)

library(data.table)
imgTestDT <- data.table( matrix(0, ncol=1, nrow=2500)  )

i <- 1
for(imgName in imgNames){
  cat("file:  ", i, "/",  numberOfImages, "\n")
  img <- readJPEG( paste0(imgDir, imgName) ) 
  imgTestDT[ , eval(imgName):= as.vector(img)] 
  i <- i + 1
}
imgTestDT[, V1:=NULL]
print(object.size(imgTestDT), units="Mb")  

# transpose before writing to file. 
imgTestDT <- t(imgTestDT)
#save(imgTestDT, file="imgTestDT.Rdata")
write.csv(imgTestDT, file="imgTestDT.csv")