# load prepared data
#-------------------------------------------------
library(data.table)
imgTrainDT <- fread(unzip("imgTrainDT_128.zip"))
#imgTrainDT <- fread("imgFeaturesTrainDT.csv")
setnames(imgTrainDT, 1, "path")
pathCol <- imgTrainDT$path
imgTrainDT[, path:=NULL]

#preprocess with caret
#--------------------------------------------------
library(caret)
nzv <- nearZeroVar(imgTrainDT)
imgTrainDT[, eval(nzv):=NULL]

col.to.scale <- names(imgTrainDT)
preProcValues <- preProcess(imgTrainDT[, .SD, .SDcols = col.to.scale ], 
                            method = c("center", "scale"))
imgTrainDT[, eval(col.to.scale):=predict(preProcValues, imgTrainDT[, .SD, .SDcols=col.to.scale]) ]


descrCor <- cor(imgTrainDT)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
imgTrainDT[, eval(highlyCorDescr):=NULL]

#######need to plot percentPCA vs cutoff
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
imgTrainDT.pca <- prcomp(imgTrainDT, center = TRUE, scale. = TRUE) 
summary(imgTrainDT.pca)
plot(imgTrainDT.pca, type='line')

#https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/

library(devtools)
install_github("ggbiplot", "vqv")

imgTrainDT[, output:=
             sapply( strsplit( pathCol, "&"), "[", 1) ]


library(ggbiplot)
g <- ggbiplot(imgTrainDT.pca, obs.scale = 1, var.scale = 1, 
              groups = output, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
