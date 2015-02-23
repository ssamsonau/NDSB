#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
statMatrixValues <- function(img){
  #Intensity values as here http://www.cellprofiler.org/CPmanual/MeasureImageIntensity.html
  c(sum(img), 
    mean(img), 
    sd(img),
    mad(img),
    quantile(img) # min, max, and quartiles ->  5 total
  )  
}  

binImStatBasic <- function(img_bin){
  NconnectedParts <- max(bwlabel(img_bin))
  Eccent <- computeFeatures.moment(img_bin)[1, "m.eccentricity"]
  Shape <-  computeFeatures.shape(img_bin)[1, ] #6
  distData <- statMatrixValues(distmap(img_bin))  
  featuresIm <- c(NconnectedParts, Eccent, Shape, distData)

  featuresIm
} 

binImStatContours <- function(img_bin){
  ####################### Contors count and curvature characteristics
  imCont <- ocontour(bwlabel(img_bin))
  contCount <- length(imCont)
  
  library(data.table)
  locCurvStatDT <- data.table(matrix(0, ncol=1, nrow=10))
  
  colNameIt <- 0
  for(cont in imCont){
    if( all(dim(cont)>1) ){ 
      locCurv <- localCurvature(cont)
      locCurvLength <- locCurv$length
      localCurvStat <- statMatrixValues(locCurv$curvature)
      
      colNameIt <- colNameIt + 1
      locCurvStatDT[, paste0("CONT", colNameIt):=c(localCurvStat, locCurvLength)]
    }    
  }
  
  if(ncol(locCurvStatDT) > 1) locCurvStatDT[, "V1":=NULL]
  
  locCurvStatDT <- data.table( t(locCurvStatDT) )
  
  CurvFeatures <- unlist( lapply(locCurvStatDT, quantile) )
  featuresIm <- c(contCount, CurvFeatures)    
  featuresIm
}

binImStat <- function(img_bin){
  featuresIm <- c()
  ######################### basic Statistics of binary image
  featuresIm <- c(featuresIm, binImStatBasic(img_bin) )
  ######################### contours Statistics of binary image
  #featuresIm <- c(featuresIm, binImStatContours(img_bin) )
  ###################fill holes
  img_filled <- fillHull(bwlabel(img_bin))
  featuresIm <- c(featuresIm, binImStatBasic(img_filled) )
  #display(img_filled)
  #display(img_bin)
  featuresIm
}


getFeatures <- function(imgIn){
  library(EBImage)
  #display(img)
  img <- 1-imgIn

  featuresIm <- c()
  #########################Intensity
  Intensity <- statMatrixValues(img)
  featuresIm <- c(featuresIm, Intensity)
  #mI <- max(img)

  ############################### COnvert to binary by 
  #find intensity to convert to binary
  intensityCutoff <- otsu(img, range = c(0, 1), levels = 256)
  img_bin <- img > intensityCutoff
  featuresIm <- c(featuresIm, binImStat(img_bin))
  featuresIm <- c(featuresIm, binImStatContours(img_bin) )
  ############################### Convert ot binary by watershed
  img_w <- watershed(img)
  img_w_bin <- img_w==1
  featuresIm <- c(featuresIm, binImStat(img_w_bin))
    
  featuresIm
}