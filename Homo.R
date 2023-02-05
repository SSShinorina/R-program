library("MASS") 
library("Matrix")
library(igraph)
library(tidyverse)  
library(corrr)
library(matrixcalc)
library(RColorBrewer)

set.seed(98989)
 
##sparse covariance
bk <- c(0:5, 7,11)
bMat <- matrix(1:8, 50, 8, byrow=TRUE)
bLis <- as.data.frame(bMat)
scov = bandSparse(50, k = bk, diag = bLis, symmetric=F)
 
##sample data
sample_mean = c(runif(50,0,1))
sample_data = mvrnorm(n=100, mu=sample_mean,Sigma=scov)
sample_data

##centering
center_scale <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

center_data=center_scale(sample_data)
center_data

##correlation
center_corr = cor(center_data,method="pearson")
center_corr

##network
center_corr[center_corr<0.2] <- 0
network <- graph_from_adjacency_matrix( center_corr, weighted=T, mode="undirected", diag=F)

coul <- brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color <- coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
plot(network,vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent") 

##without centering
wc_corr = cor(sample_data,method="pearson")
wc_corr
wc_network <- graph_from_adjacency_matrix( center_corr, weighted=T, mode="undirected", diag=F)

coul <- brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color <- coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
plot(wc_network,vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent") 
