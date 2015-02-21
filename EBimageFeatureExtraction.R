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

binImStat <- function(img){
  NconnectedParts <- max(bwlabel(img))
  Eccent <- computeFeatures.moment(img)[1, "m.eccentricity"]
  Shape <-  computeFeatures.shape(img)[1, ] #6
  distData <- statMatrixValues(distmap(img))  
  featuresIm <- c(NconnectedParts, Eccent, Shape, distData)
  
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

  ############################### COnvert to binary
  #find intensity to convert to binary
  intensityCutoff <- otsu(img, range = c(0, max(img)), levels = 256)
   
  img_bin <- img > intensityCutoff
  
  ######################### basic Statistics of binary image
  featuresIm <- c(featuresIm, binImStat(img_bin) )
  
  ####################### Contors count and curvature characteristics
  imCont <- ocontour(bwlabel(img_bin))
  contCount <- length(imCont)
  
  library(data.table)
  locCurvStatDT <- data.table(matrix(0, ncol=1, nrow=10))
    
  #statCurvValues <- function(img){
   # c(sum(img), mean(img), quantile(img) ) # min, max, and quartiles ->  5 total
  #}
  
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
  featuresIm <- c(featuresIm, contCount, CurvFeatures)    
  ###################fill holes
  img_filled <- fillHull(bwlabel(img_bin))
  featuresIm <- c(featuresIm, binImStat(img_filled) )
  #display(img_filled)
  #display(img_bin)

  
  featuresIm
}