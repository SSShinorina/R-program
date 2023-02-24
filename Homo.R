library("MASS") 
library("Matrix")
library(igraph)
library(tidyverse)  
library(corrr)
library(matrixcalc)
library(RColorBrewer)
library("cluster")

### creating block sparse matrix with 3 different block matrices.
set.seed(1)
a=forceSymmetric(matrix(5:20,15,15))
b=forceSymmetric(matrix(30:40,10,10))
c=forceSymmetric(matrix(50:60,25,25))
sM=bdiag(a,b,c)
sM
##sample data
sample_mean = c(runif(50,0,1))
sample_data = mvrnorm(n=100, mu=sample_mean,Sigma=sM,tol=.6)
sample_data

##centering
center_scale <- function(y) {
  apply(y, 2, function(x) x - mean(x))
}

center_data=center_scale(sample_data)
center_data

##correlation
center_corr = cor(center_data,method="pearson")
center_corr

##network
center_corr[center_corr<0.9] <- 0
network <- graph_from_adjacency_matrix( center_corr, weighted=T, mode="undirected", diag=F)

coul <- brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color <- coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
plot(network,vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent") 

ceb1 = cluster_edge_betweenness(network)

##without centering
wc_corr = cor(sample_data,method="pearson")
wc_corr
wc_corr[wc_corr<0.9] <- 0
wc_network <- graph_from_adjacency_matrix( wc_corr, weighted=T, mode="undirected", diag=F)

coul <- brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color <- coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
plot(wc_network,vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent")
 
ceb = cluster_edge_betweenness(wc_network)
membership(ceb)
membership(ceb1)
plot(ceb,wc_network)
plot(ceb1,network)









########################################################################################################
################# heterogenus ######################

for (i in nrow(sample_data)-1){
   a = runif(100,min=0,max=50)
   new_sample = sample_data[sample_data[i]+a,]
}

new_sample
##centering
center_scale <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

center_data=center_scale(new_sample)
center_data

##correlation
center_corr = cor(center_data,method="pearson")
center_corr

##network
center_corr[center_corr<0.9] <- 0
network <- graph_from_adjacency_matrix( center_corr, weighted=T, mode="undirected", diag=F)

coul <- brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color <- coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
plot(network,vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent") 

ceb1 = cluster_edge_betweenness(network)

##without centering
wc_corr = cor(new_sample,method="pearson")
wc_corr
wc_corr[wc_corr<0.9] <- 0
wc_network <- graph_from_adjacency_matrix( wc_corr, weighted=T, mode="undirected", diag=F)

coul <- brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color <- coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
plot(wc_network,vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent")

ceb = cluster_edge_betweenness(wc_network)
membership(ceb)
membership(ceb1)

