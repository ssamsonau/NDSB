#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
binImStatBasic <- function(img_bin){
  Eccent <- computeFeatures.moment(img_bin)[1, "m.eccentricity"]
  Shape <-  computeFeatures.shape(img_bin)[1, ] #6
  featuresIm <- c(Eccent, Shape)

  featuresIm
} 

RadialFeatures <- function(img, NumSplits){
  NumSplits <- NumSplits+1
  featuresIm <- c()
  totalIntencity <- sum(img)
  
  ind.of.pixels <- which(img>0, arr.ind = T) 
  #find  center of mass
  library(EBImage)
  #chosen <- img>otsu(img)
  #newImage <- img 
  #newImage <- newImage * chosen
  
  x <- ind.of.pixels[, 1]
  y <- ind.of.pixels[, 2]
    
  cm.x <- round( sum( img[ind.of.pixels] * x )  / sum(img[ind.of.pixels]) )
  cm.y <- round( sum( img[ind.of.pixels] * y )  / sum(img[ind.of.pixels]) )
  
  radius.m <- matrix(NA, nrow=nrow(img), ncol=ncol(img))

  ind.of.center <- matrix(rep(c(cm.x, cm.y), dim(ind.of.pixels)[1] ), ncol=2, byrow = T) 
  diference <- ind.of.pixels - ind.of.center
  #distance from the center of mass
  radius <- sqrt(rowSums(diference^2))
  radius.m[ind.of.pixels] <-  radius
  
  #find ratio of mass within certain radius interval 
  r.splits <- seq(0, max(radius), length.out = NumSplits)
  
  for(i in 2:length(r.splits)){
    match.radius.m <- r.splits[i-1] < radius.m  & radius.m < r.splits[i] 
    featuresIm <- c(featuresIm, 
                    sum ( img[ which(match.radius.m, arr.ind =T)]  ) / totalIntencity )
  }  
  featuresIm
}

getFeatures <- function(imgIn, Splits=10){
  library(EBImage)
  2#display(img)
  img <- 1-imgIn

  featuresIm <- c()
  #Radial features addition
  featuresIm <- c(featuresIm, RadialFeatures(img, Splits))
  ############################### COnvert to binary all non zero
  img_bin <- img > 0
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  
  ############################### COnvert to binary by otsu 
  #find intensity to convert to binary
  img_bin <- img >   otsu(img)
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  
  #features for filled Image
  filled.img <- fillHull( img>otsu(img) )
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  featuresIm <- c(featuresIm, RadialFeatures(filled.img, Splits))
  
  featuresIm
}