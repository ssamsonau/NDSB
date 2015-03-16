packages.to.load <- c("data.table", "caret","randomForest")

for(p in packages.to.load){
  if(! p %in% installed.packages()){
    install.packages(p)
  }
}
 
############################################

source("predict.R")

