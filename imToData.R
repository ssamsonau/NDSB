rootDataDir <- "E://Temp/NDSB/train/"

folderNames <- dir(rootDataDir) 
#folderNames <- grep("unknown", folderNames, invert=TRUE, value=TRUE)

typeNames <- c()
for(folderName in folderNames){
  dirContent <- dir(paste0(rootDataDir, folderName))
  typeNames <- c(typeNames, rep(folderName, length(dirContent)))
}
output <- factor(typeNames)

library(jpeg)
library('Matrix') 
imgMatix <- Matrix(0, nrow = length(typeNames), ncol = 2500, sparse = TRUE)
#ImgMatix <- matrix(0, nrow = length(typeNames), ncol = 2500)
i <- 1

for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- grep("r50_", dir(imgDir), value=TRUE)
  for(imgName in imgNames){
    img <- readJPEG( paste0(imgDir, imgName) )
    imgMatix[i, ] <- as.vector(img)
    i <- i + 1
    cat("file:  ", paste0(imgDir, imgName), "\n")
    print(object.size(imgMatix), units="Mb")    
  }
} 

save(imgMatix, file="imgMatix.Rdata")
save(output, file="output.Rdata")
  