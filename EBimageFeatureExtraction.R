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

largest_connected <- function(img_bin){
  ################binary features for largest connected
  featuresIm <- c()
  
  connections_mapping <- bwlabel(img_bin)
  number_of_connected_parts <- max(connections_mapping)
  featuresIm <- c(featuresIm, number_of_connected_parts)
  
  connected_freq <- table( connections_mapping[connections_mapping!=0] )
  label_of_largest <-as.numeric( names(connected_freq)[connected_freq == max(connected_freq)] )
  img_bin_sub <- (bwlabel(img_bin) %in% label_of_largest) * img_bin 
  # should be %in% instead of "==" , for situation when there is equal number of different labels 
  featuresIm <- c(featuresIm, binImStatBasic(img_bin_sub))
  
  featuresIm
}

getFeatures <- function(imgIn, Splits=10){
  library(EBImage)
  #display(img)
  img <- 1-imgIn

  featuresIm <- c()
  #sd of intencity
  featuresIm <- c(featuresIm, sd(img))
  
  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img, Splits))
  featuresIm <- c(featuresIm, RadialFeatures(img, Splits*2))
  ####################################
  ############################### COnvert to binary all non zero
  img_bin <- img > 0
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  featuresIm <- c(featuresIm, largest_connected(img_bin))

  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits))
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits*2))
  
  ###############################
  ############################### COnvert to binary by otsu 
  img_bin <- img >   otsu(img)
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  
  featuresIm <- c(featuresIm, largest_connected(img_bin))
  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits))
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits*2))
    
  ######################################
  ###features for filled Image
  img_bin <- fillHull( img>otsu(img) )
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  featuresIm <- c(featuresIm, largest_connected(img_bin))
  
  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits))
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits*2))
  
  ##############
  featuresIm
}