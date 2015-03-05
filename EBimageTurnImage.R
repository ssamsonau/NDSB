#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#display(turnImage( readJPEG("E://Temp/NDSB/train/amphipods/109887.jpg"), 30))
turnImage <- function(img, size_im = 100){
  library(jpeg)
  library(EBImage)
  #display(img)
  #inverse
  img <- 1-img
  
  #kern = makeBrush(3, shape='diamond')
  #img <-  dilate(img, kern)
  #moments <- computeFeatures.moment(img3)[1, ]

  #resize
  if(dim(img)[1] > dim(img)[2]){
    img <- resize(img, w=size_im, output.dim = c(size_im, size_im))  
  } else{
    img <- resize(img, h=size_im, output.dim = c(size_im, size_im))
  }
  
  #make binary to calculate turning param
  img_bin <- img > otsu(img)
  #take only largest connected part to calulate turning parameters
  connections_mapping <- bwlabel(img_bin)
  connected_freq <- table( connections_mapping[connections_mapping!=0] )
  label_of_largest <-as.numeric( names(connected_freq)[connected_freq == max(connected_freq)] )
  img_bin_sub <- (bwlabel(img_bin) == label_of_largest) * img_bin 
    
  #turn
  moments <- computeFeatures.moment(img_bin_sub)[1, ]
  if(! is.null(moments) ){  # sometimes programm can not find moments... then moments is NULL
    img <- translate(img, v = -c(size_im/2, size_im/2) + moments[c("m.cx", "m.cy")])
    img <- rotate(img, -moments["m.theta"]*360/6.28)
  }
  #display(img)  
  
  #make binary and take only largest connected part
  img_bin <- img > otsu(img)
  connections_mapping <- bwlabel(img_bin)
  connected_freq <- table( connections_mapping[connections_mapping!=0] )
  label_of_largest <-as.numeric( names(connected_freq)[connected_freq == max(connected_freq)] )
  img_bin_sub <- (bwlabel(img_bin) == label_of_largest) * img_bin 
  
  img <- img_bin_sub
  img
}