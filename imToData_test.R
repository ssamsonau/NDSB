rootDataDir <- "E://Temp/NDSB/test/"
imgDir <- rootDataDir

library(jpeg)
library('Matrix') 
numberOfImages <- length(dir(imgDir)[1:50])
imgMatix_test <- Matrix(0, nrow = numberOfImages, ncol = 2500, sparse = F)
#ImgMatix <- matrix(0, nrow = length(typeNames), ncol = 2500)
i <- 1

imgNames <- grep("r50_", dir(imgDir), value=TRUE)[1:numberOfImages]
for(imgName in imgNames){
  system.time( img <- readJPEG( paste0(imgDir, imgName) ) )
  system.time( imgMatix_test[i, ] <- as.vector(img) )
  i <- i + 1
  cat("file:  ", i, "/",  numberOfImages, "\n")
}

print(object.size(imgMatix_test), units="Mb")  

save(imgMatix_test, file="imgMatix_test_dense.Rdata")

imgNames_original <- sapply(strsplit(imgNames, "_"), "[", 2) 
save(imgNames_original, file="imgNames_test.Rdata")
