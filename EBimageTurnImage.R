#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
turnImage <- function(img, sizeIm = 100){
  library(jpeg)
  library(EBImage)
  #display(img)
  img <- 1-img
  kern = makeBrush(3, shape='diamond')
  img <-  dilate(img, kern)
  #moments <- computeFeatures.moment(img3)[1, ]
  if(dim(img)[1] > dim(img)[2]){
    img <- resize(img, w=sizeIm, output.dim = c(sizeIm, sizeIm))  
  } else{
    img <- resize(img, h=sizeIm, output.dim = c(sizeIm, sizeIm))
  }
  
  img_temp <- img > 0.1
  moments <- computeFeatures.moment(img_temp)[1, ]
  if(! is.null(moments) ){  # sometimes programm can not find moments... then moments is NULL
    img <- translate(img, v = -c(sizeIm/2, sizeIm/2) + moments[c("m.cx", "m.cy")])
    img <- rotate(img, -moments["m.theta"]*360/6.28)
  }
  img <- img > 0.1
  #display(img)
  #img <- ifelse(img, 1, 0)
  img
}