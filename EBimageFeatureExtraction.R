#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")


getFeatures <- function(img, numOfSlices = 100){
  library(EBImage)
  #display(img)
  img <- 1-img
  
  featuresIm <- c()
  
  #slice should not start from 0 - such slics is useless - only black
  
  for(i in seq(1/numOfSlices, 1, length.out= numOfSlices) ){ 
    img_temp <- img >= i  
    Eccent <- computeFeatures.moment(img_temp)[1, "m.eccentricity"]
    Shape <-  (computeFeatures.shape(img_temp)[1, ])
    featuresIm <- c(featuresIm, Eccent, Shape)
  }
  #display(img_temp)

  featuresIm
}