rootDataDir <- "E://Temp/NDSB/test/"

imgDir <- rootDataDir
cat("folder: ", imgDir, "\n")  
imgNames <- dir(imgDir)

for(imgName in imgNames){
  system2("C://cygwin64/bin/convert.exe",  
          paste0(imgDir, imgName,
                 " -resize 50x50 -negate -background black -gravity Center -extent 50x50 "
                 , imgDir, "r50_",  imgName), wait=TRUE)  
  cat("file:  ", paste0(imgDir, imgName), "\n")
}

