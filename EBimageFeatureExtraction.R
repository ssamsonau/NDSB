#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
binImStatBasic <- function(img_bin){
  Eccent <- computeFeatures.moment(img_bin)[1, "m.eccentricity"]
  Shape <-  computeFeatures.shape(img_bin)[1, ] #6
  featuresIm <- c(Eccent, Shape)

  featuresIm
} 

getFeatures <- function(imgIn){
  library(EBImage)
  #display(img)
  img <- 1-imgIn

  featuresIm <- c()
  ############################### COnvert to binary all non zero
  img_bin <- img > 0
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  
  ############################### COnvert to binary by otsu 
  #find intensity to convert to binary
  intensityCutoff <- otsu(img, range = c(0, 1), levels = 256)
  img_bin <- img > intensityCutoff
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
    
  featuresIm
}