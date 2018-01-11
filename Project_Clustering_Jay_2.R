library(caret)
library(snn)
library(dbscan)
library(plotly)
options(warn=-1)

#KMeans Algorithm for Large Dataset
set.seed(3456)
InputData2 <- read.csv("dataset2.csv")
kdata <- kmeans(InputData2,121,nstart=23)
kdata


#Identifying Root Mean Squae Deviation

rmsd <- function(result_error)
{
  sqrt(mean(result_error^2))
}


result_error <- vector()
for(i in kdata[1]){
    cx = kdata$centers[kdata$cluster[i],1]
    cy = kdata$centers[kdata$cluster[i],2]
    cz = kdata$centers[kdata$cluster[i],3]
    cw = kdata$centers[kdata$cluster[i],4]
    
    ex = (InputData2$x[i] - cx)^2
    ey = (InputData2$y[i] - cy)^2
    ez = (InputData2$z[i] - cz)^2
    ew = (InputData2$w[i] - cw)^2
    result_error <- c(result_error, sqrt((ex + ey + ez + ew)))
  }
  
rmsd(result_error)

