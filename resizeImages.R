rootDataDir <- "E://Temp/NDSB/train/"

folderNames <- dir(rootDataDir) 

#system2("C://cygwin64/bin/convert.exe", "dragon.gif
#        -resize 50x50 -negate -background red  -gravity Center -extent 50x50 1.jpg" )

#system2("C://cygwin64/bin/ls.exe", "-a")

#Use linux imageMagic to 
# resize (fitt in to desired size without changing the ratio), 
# negate (most of the picture will be 0 instead of 1)
# center
# fill background with black (0)
# 
for(folderName in folderNames){
  imgDir <- paste0(rootDataDir, folderName, "/")
  cat("folder: ", imgDir, "\n")  
  
  imgNames <- dir(imgDir)
  for(imgName in imgNames){
    system2("C://cygwin64/bin/convert.exe",  
            paste0(imgDir, imgName,
                   " -resize 50x50 -negate -background black -gravity Center -extent 50x50 "
                   , imgDir, "r50_",  imgName), wait=TRUE)  
    cat("file:  ", paste0(imgDir, imgName), "\n")
  }
} 
