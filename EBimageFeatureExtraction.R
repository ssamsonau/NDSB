#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")


getFeatures <- function(img, numOfSlices = 100){
  library(EBImage)
  #display(img)
  img <- 1-img
  
  featuresIm <- c()
  
  for(i in seq(0, 1-1/numOfSlices, length.out= numOfSlices)  ){ 
    img_temp <- img >= i  
    Eccent <- computeFeatures.moment(img_temp)[1, "m.eccentricity"]
    
    Shape <-  (computeFeatures.shape(img_temp)[1, ])
    
    if(is.null(Eccent) | is.null(Shape)) {
      Eccent <- c(0) # this is what by default is assigned for 1 pixel #prev.E
      Shape <- c(1, 1, 0, 0, 0, 0) # this is what by default is assigned for 1 pixel  #prev.S    
    }
    #prev.E <- Eccent
    #prev.S <- Shape
    
    featuresIm <- c(featuresIm, Eccent, Shape)
    
    
  }
  #display(img_temp)

  featuresIm
}