rm(list=ls())
rootDataDir <- "E://Temp/NDSB/train/"
folderNames <- dir(rootDataDir) 
#------------
numberOfImages <- 0
for(folderName in folderNames){
  numberOfImages <- numberOfImages + length(dir(paste0(rootDataDir, folderName)))
}


library(jpeg)
#img <- readJPEG("E:/Temp/NDSB/train/acantharia_protist_big_center/111204.jpg")
#img <- 1-img
#img
#system2("C:/cygwin64/bin/convert.exe", "dragon.gif \
#        -distort Barrel '-0.2 0.0 0.0 1.3'   barrel_checks_A.png")

distort_im <- function(imgDir, imgName, n_st, ope){
  system2("C:/cygwin64/bin/convert.exe", 
          paste0(imgDir, imgName, " -virtual-pixel white ", ope, imgDir, n_st ,imgName) )  
}

dist_series_of_img <- function(imgDir, imgName, number_of_new){
  library(jpeg)
  img <- readJPEG(paste0(imgDir, imgName))
  x <- dim(img)[1]
  y <- dim(img)[2]
  
  r <- function(x) round(x)
  
  distort_im(imgDir, imgName, "D1_", 
             ope = paste0("-distort Perspective",
                          " '0,0,0,0",         " "            ,"0,",y,",0,",y," ",
                          x,",0,",x,",",r(y/4)," "      , x,",",y,",",x,",",r(y-y/4),"  ' "))
  
  if(number_of_new >=2)    
    distort_im(imgDir, imgName, "D2_", 
               ope = paste0("-distort Perspective",
                            " '0,0,", r(x/4),",0",       " "    ,"0,",y,",0,",y," ",
                            x,",0,", r(x-x/4),",0",     "  "    , x,",",y,",",x,",",y,"  ' ")    )  
  
  if(number_of_new >=3)    
    distort_im(imgDir, imgName, "D3_", 
               ope = paste0("-distort Perspective",
                            " '0,0,0,0",       " "    ,"0,",y,",",r(x/4),",",y," ",
                            x,",0,", x,",0",     "  "    , x,",",y,",",r(x-x/4),",",y,"  ' ")    )  
  
  if(number_of_new >=4)    
    distort_im(imgDir, imgName, "D4_", 
               ope = paste0("-distort Perspective",
                            " '0,0,0,",r(y/4),       " "    ,"0,",y,",0,",r(y-y/4)," ",
                            x,",0,", x,",0",     "  "    , x,",",y,",",x,",",y,"  ' ")    )  
  if(number_of_new >=5)
    distort_im(imgDir, imgName, "D5_", 
             ope = paste0(" -distort Barrel '0.7 0.0 0.0 1.0' ") )
  
  #if(number_of_new >=6)
  #  distort_im(imgDir, imgName, "D6_", 
  #             ope = paste0(" -distort Barrel '-0.2 -0.0 0.0 1.0' ") )

}

i <- 1 
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")    
  imgNames <- grep("D", dir(imgDir), value=T, invert=T)
  if(length(imgNames) < 200 ){
    
    for(imgName in imgNames){
      cat("file:  ", i, "/",  numberOfImages, "\n")
      
      dist_series_of_img(imgDir, imgName, round( 200/length(imgNames) ) )

      i <- i + 1
    }  
  }else i<- i+length(imgNames)
  
} 
