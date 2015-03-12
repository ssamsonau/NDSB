# Set MKL threads for Revolution R Open
if(require(Revobase)) {library(doParallel); setMKLthreads(detectCores())};

library(data.table)
#
dom <- T #calculated on domino?

if(dom==T) {dir_root = "./"
}else {dir_root = "E:/Temp/forest/"}

dir_models=paste0(dir_root, "separate/")
models_names <- dir(dir_models)

############ Load first model, obtained by caret
load(paste0(dir_models, models_names[1]))
Fit_caret <- Fit$finalModel
rm(Fit)
models_names <- models_names[-1]

########## Prepare parallel execution
if(dom==T){num_of_workers <- 8
}else{library(doParallel);  num_of_workers <- detectCores()}

if(dom==T){
  #parallel in Unix
  require('doMC');  registerDoMC(num_of_workers)  
}else{
  #parallel in Windows
  library(doParallel);  cl <- makeCluster(detectCores());  registerDoParallel(cl)
}

models_names_dt <- data.table(models_names)
models_names_dt[, sep:= 1:num_of_workers]

library(foreach); library(randomForest)

Fit_rf_combined <- foreach(k=1:num_of_workers, .combine=combine, .packages='randomForest') %dopar% {
 
  library(data.table)
  names <- models_names_dt[sep==k, models_names]
  load(paste0(dir_models, names[1]))
  comb_fit <- Fit
  names <- names[-1]
  
  for(name in names){
    #print(paste0("working with ", name))
    load(paste0(dir_models, name))
    comb_fit <- combine(comb_fit, Fit)
  }
  comb_fit
}

Fit_rf_combined <- combine(Fit_caret, Fit_rf_combined)

print(Fit_rf_combined)

if(dom==T){ imgTestDT <- fread(unzip(paste0(dir_root, "316featuresTest_imp.zip"))) 
}else{imgTestDT <- fread(paste0(dir_root, "data/316featuresTest_imp.csv"))}

filenameCol <- imgTestDT[, .filename]
imgTestDT[, .filename:=NULL]
#make prediction of output
#--------------
library(randomForest)
predicted <- predict(Fit_rf_combined, newdata=imgTestDT, type="prob")

#form a submission data table
resultsDT <- data.table(predicted)

#when I do subset of data, not all types are presented in prediction. Let us manyally 
#add them to ouptut with prob of 0

submissionVect <- unlist( read.csv(unzip("sampleSubmission.csv.zip"), nrow=1, header=F) )
submissionVect <- sub("-", "_", submissionVect)
missing <- submissionVect[ ! submissionVect %in% names(resultsDT) ]

resultsDT[, eval(missing):=0]

resultsDT[, image:=filenameCol]
new_order <- match(submissionVect, names(resultsDT))
setcolorder(resultsDT, new_order)

write.csv(resultsDT, file="to_be_submitted.txt", quote=F, row.names=F )

if(dom==F){
  system2("C://cygwin64/bin/sed.exe", " -i 's/shrimp_like/shrimp-like/g' to_be_submitted.txt")  
  system2("C://Program Files/7-Zip/7z.exe", "a -tzip to_be_submitted.zip to_be_submitted.txt")
}
