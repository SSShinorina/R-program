library("MASS") 
library("Matrix")
library(igraph)
library(tidyverse)  
library(corrr)
library(corrplot)
library(matrixcalc)
library(RColorBrewer)
library("cluster")
library(pdfCluster)
library(ggplot2)
library(psych)

### creating block sparse matrix with 3 different block matrices.
a=forceSymmetric(matrix(2:50,20,20))
b=forceSymmetric(matrix(10:40,20,20))
c=forceSymmetric(matrix(5:7,10,10))
a
b
c
sM=bdiag(a,b,c)
sM
plot(sM)

true_cor = cov2cor(sM)
true_network <- graph_from_adjacency_matrix( true_cor, weighted=T, mode="undirected", diag=F)
true_member = cluster_edge_betweenness(true_network)
dev.off()
par(bg="black", mar=c(0,0,0,0))
plot(true_network,vertex.size=12,
     vertex.color="green", 
     vertex.label.cex=0.7,
     vertex.label.color="blue",
     vertex.frame.color="transparent") 
membership(true_member) 
plot(true_member,true_network)

##sample data
set.seed(123)
sample_mean = c(runif(50,0,0))
sample_data = mvrnorm(n=100, mu=sample_mean,Sigma=sM,tol=.6)
plot(sample_data)
heatmap(sample_data)

sample_data = data.frame(sample_data)
sample_data 

##centering
center_data <- function(x) {
  apply(x, 2, function(y) y - mean(y))
} 
center_data = center_data(sample_data)


##correlation
center_corr = cor(center_data,method="pearson")
center_corr 

##network
center_corr[center_corr<0.9]<-0
center_corr[center_corr>=0.9]<-1
heatmap(center_corr)
dev.off()
corrplot(center_corr , addCoef.col="pink",number.cex=0.55)
network <- graph_from_adjacency_matrix( center_corr, weighted=T, mode="undirected", diag=F)

dev.off()
par(bg="black", mar=c(0,0,0,0))
plot(network,vertex.size=12,
     vertex.color="green", 
     vertex.label.cex=0.7,
     vertex.label.color="blue",
     vertex.frame.color="transparent") 

center_member = cluster_edge_betweenness(network)

##without centering
wc_corr = cor(sample_data,method="pearson")
wc_corr
wc_corr[wc_corr<0.9] <- 0
wc_corr[wc_corr>=0.9] <- 1
dev.off()
corrplot(wc_corr , addCoef.col="pink",number.cex=0.55)
wc_network <- graph_from_adjacency_matrix( wc_corr, weighted=T, mode="undirected", diag=F)

dev.off()
par(bg="black", mar=c(0,0,0,0))
plot(wc_network,vertex.size=12,
     vertex.color="green", 
     vertex.label.cex=0.7,
     vertex.label.color="blue",
     vertex.frame.color="transparent")

wc_member = cluster_edge_betweenness(wc_network)
membership(wc_member)
membership(center_member)
plot(wc_member,wc_network)
plot(center_member,network) 

adj.rand.index(membership(true_member),membership(center_member))
adj.rand.index(membership(true_member),membership(wc_member))
