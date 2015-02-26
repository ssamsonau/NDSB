packages.to.load <- c("data.table", "caret")

for(p in packages.to.load){
  if(! p %in% installed.packages()){
    install.packages(p)
  }
}

############################################

source("make_model.R")

