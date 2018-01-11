library(caret)
library(snn)
library(dbscan)
library(plotly)
library(ClusterR)
options(warn=-1)


###############----------Reading the Input Data--------------######################
InputData <- function() {
  InputData <- read.csv("dataset1.csv") 
  return (InputData)
}

##############-----------Heirarchical Clustering----------######################
hcclust <- function(InputData) {
  hc <- hclust(dist(InputData, method="euclidean"), method="ward.D2")
  clusterCut <- cutree(hc, 8)
  return(clusterCut)
}

###############----------K-Means Clustering--------------######################
kmeansclust <- function(x,InputData) {
  set.seed(x)
  kc <- kmeans(InputData,8,nstart=2)
  return(kc)
}

###############-----------Density Based Clustering--------------################
dbclust <- function(InputData) {
  db <- dbscan::dbscan(InputData, eps=1.6, MinPts = 10)
  return(db)
}

###############-----------Graph Based Clustering---------------################
gbclust <- function (InputData) {
  gb <- sNNclust(dist(InputData), k = 20, eps = 10, minPts = 16)
  return(gb)
}

#############-------------Plotting the 3D Results-------------################
clustresultgraph <- function(InputData,Input) {
  predictedResult <- data.frame(InputData,Input)
  p <- plot_ly(predictedResult, x = ~x, y = ~y, z = ~z, color = ~Input) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'X'),
                        yaxis = list(title = 'Y'),
                        zaxis = list(title = 'Z')))
  return(p)
  
}


################-------------Accuracy Check--------------------################
ExternalValidation <- function(InputData,Cluster) {
  valdation <- external_validation(InputData, Cluster, method = "adjusted_rand_index",
                                   summary_stats = TRUE)
}



InputData <- InputData()       #Taking Input Data


#######---------------------------------------Heirarchical Clustering Implementation
hc <-  hcclust(InputData[,1:3])
hvalidation <-  ExternalValidation(InputData[,4], hc)
heirarchical_plot <- clustresultgraph(InputData[,1:3],hc)
heirarchical_plot

#######---------------------------------------KMeans Clustering Implementation
kc <- kmeansclust(999,InputData[,1:3])
kvalidation <- ExternalValidation(InputData[,4], kc$cluster)
kmeans_plot <- clustresultgraph(InputData[,1:3],kc$cluster)
kmeans_plot

#######---------------------------------------Desnity Based Clustering Implementation 
dbc <- dbclust(InputData[,1:3])
dvalidation <- ExternalValidation(InputData[,4], dbc$cluster)
densityclust_plot <- clustresultgraph(InputData[,1:3],dbc$cluster)
densityclust_plot

#######---------------------------------------Graph Based Clustering Implementation 
gbc <- gbclust(InputData[,1:3])
gvalidation <- ExternalValidation(InputData[,4], gbc$cluster)
graphclust_plot <- clustresultgraph(InputData[,1:3],gbc$cluster)
graphclust_plot


