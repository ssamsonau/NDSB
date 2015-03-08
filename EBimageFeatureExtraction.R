#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
binImStatBasic <- function(img_bin){
  Moments <- computeFeatures.moment(img_bin)[1, c("m.majoraxis","m.eccentricity")]
  Eccent <- Moments["m.eccentricity"]
  semi_major_axis <- Moments["m.majoraxis"] / 2 
  semi_minor_axis <- sqrt( (1-Eccent^2)*semi_major_axis^2 )
  elipse_area <- pi * semi_major_axis * semi_minor_axis
  
  Shape <-  computeFeatures.shape(img_bin)[1, ] #6
  area_to_per <- Shape["s.area"]/Shape["s.perimeter"]
  # Shape["s.area"] is actually count of pixels - so is jus a mass
  density <- Shape["s.area"]/elipse_area

  featuresIm <- c(Eccent, Shape, elipse_area, area_to_per, density)
  featuresIm
} 

RadialFeatures <- function(img, NumSplits=10){
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
  angle.m <- matrix(NA, nrow=nrow(img), ncol=ncol(img))
  
  ind.of.center <- matrix(rep(c(cm.x, cm.y), dim(ind.of.pixels)[1] ), ncol=2, byrow = T) 
  diference <- ind.of.pixels - ind.of.center
  #distance from the center of mass
  radius <- sqrt(rowSums(diference^2))
  radius.m[ind.of.pixels] <-  radius
  
  angle <- atan2(diference[,2], diference[, 1])*180/pi

  #adjust angle relative to major axis
  moments <- computeFeatures.moment(img>0)[1, ]
  if(! is.null(moments) ){  # sometimes programm can not find moments... then moments is NULL
    angle <- angle - moments["m.theta"]*180/pi
    ifelse(angle>180, angle-360, angle)
    ifelse(angle<-180, angle+360, angle)
  }
  
  angle.m[ind.of.pixels] <-  angle
  
  mass_by_angle_f <- function(a_split_number, match.radius.m){
    mass_by_angle <- c()    
    
    a.splits <- seq(-180, 180, length.out=a_split_number)
        
    for(i in 2:length(a.splits)){
      #logic matrix of matching angles
      match.angle.m <- a.splits[i-1] <= angle.m  & angle.m < a.splits[i]
      chosen.indexes.m <- which( match.angle.m * match.radius.m == 1, arr.ind=T )
      sum_for_rad <- sum(img[ which( match.radius.m==1, arr.ind =T ) ])
      
      sum_at_chosen_a <- sum ( img[chosen.indexes.m] ) / 
        ifelse(sum_for_rad, sum_for_rad, 1)#remove division by 0

      mass_by_angle <- c(mass_by_angle, sum_at_chosen_a)  
    }  
    
    featuresIm <- c(mass_by_angle, sd(mass_by_angle) )
    featuresIm    
  }

  # angle features - no radius splitting
  match.radius.m_gl <- matrix(NA, nrow=nrow(img), ncol=ncol(img) ) 
  match.radius.m_gl[ind.of.pixels] <- T

  #featuresIm <- c(featuresIm, mass_by_angle_f(2, match.radius.m_gl) )
  featuresIm <- c(featuresIm, mass_by_angle_f(4, match.radius.m_gl) )
  featuresIm <- c(featuresIm, mass_by_angle_f(20, match.radius.m_gl) )
  #featuresIm <- c(featuresIm, mass_by_angle_f(40, match.radius.m_gl) )
  
  #find ratio of mass within certain radius interval 
  r.splits <- seq(0, max(radius), length.out = NumSplits)
  
  r_sd <- c() 
  for(i in 2:length(r.splits)){
    match.radius.m <- r.splits[i-1] <= radius.m  & radius.m < r.splits[i] 
    r_m <- sum ( img[ which(match.radius.m, arr.ind =T)]  ) / totalIntencity
    featuresIm <- c(featuresIm, r_m )
    r_sd <- c(r_sd, r_m) 
    
    # angle features for each radius
    #featuresIm <- c(featuresIm, mass_by_angle_f(20, match.radius.m) )
  }  
  featuresIm <- c(featuresIm, sd(r_sd) ) 
  
  featuresIm
}

largest_connected <- function(img_bin, Splits=10){
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

  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin_sub, Splits))
  #featuresIm <- c(featuresIm, RadialFeatures(img_bin_sub, 2*Splits))
  
  
  featuresIm
}

getFeatures <- function(imgIn, Splits=10){
  library(EBImage)
  #display(img)
  img <- 1-imgIn

  featuresIm <- c()
  #count all pixels
  featuresIm <- c(featuresIm, sum(img>0) )
  #sd of intencity
  non_zero_im <- img[img!=0]
  featuresIm <- c(featuresIm, sd(non_zero_im), sum(non_zero_im), quantile(non_zero_im))
  
  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img, Splits))
  #featuresIm <- c(featuresIm, RadialFeatures(img, Splits*2))
  ####################################
  ############################### COnvert to binary all non zero
  img_bin <- img > 0
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  featuresIm <- c(featuresIm, largest_connected(img_bin, Splits))

  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits))
  #featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits*2))
  
  ###############################
  ############################### COnvert to binary by otsu 
  img_bin <- img >   otsu(img)
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  
  featuresIm <- c(featuresIm, largest_connected(img_bin, Splits))
  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits))
  #featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits*2))
    
  ######################################
  ###features for filled Image
  img_bin <- fillHull( img>otsu(img) )
  featuresIm <- c(featuresIm, binImStatBasic(img_bin))
  featuresIm <- c(featuresIm, largest_connected(img_bin, Splits))
  
  #Radial features
  featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits))
  #featuresIm <- c(featuresIm, RadialFeatures(img_bin, Splits*2))
  
  ##############
  featuresIm
}