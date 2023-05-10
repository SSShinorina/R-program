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
library(pheatmap)
library(BlockCov)

### creating block sparse matrix with 3 different block matrices.

n <- 15
X <- matrix(runif(n^2)*2-1 , ncol=n)
X_centered <- scale(X, center = TRUE, scale = FALSE)
A <- X_centered
Sigma <- t(A) %*% A / (n - 1) 
Sigma

n <- 10
X <- matrix(runif(n^2)*2-1 ,ncol=n)
X_centered <- scale(X, center = TRUE, scale = FALSE)
B<- X_centered
SigmaB <- t(B) %*% B/(n-1) 
SigmaB
n <- 25
X <- matrix(runif(n^2)*2-1,ncol=n)
X_centered <- scale(X, center = TRUE, scale = FALSE)
C <- X_centered
SigmaC <- t(C) %*% C/(n-1) 
SigmaC
 

sM= bdiag(Sigma,SigmaB,SigmaC)
plot(sM)
sM=as.matrix(sM)
sM

true_cor = cov2cor(sM)
true_cor
corrplot(true_cor,number.cex=0.5) 
true_cor[true_cor<0]<-0

true_network <- graph_from_adjacency_matrix( true_cor, weighted=T, mode="undirected", diag=F)
plot(true_network)
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

####### true covariance number##############
dev.off()
pheatmap(sM,color = colorRampPalette(rev(brewer.pal(n = 50, name =
                                                      "Set2")))(100),display_numbers=T, show_colnames=T, cluster_rows=F, cluster_cols=F)

###################### calculating cut point ###############
## correlation ordering
l = table(true_cor)
l=l[names(l)==1]
l
f=l/950
f
cut_point = 2500*f
cut_point 

## skeleton 
true_cor[true_cor>0]<-1
true_cor[true_cor<0]<-0
true_cor
dev.off()
corrplot(true_cor,number.cex=0.5)

corrplot(true_cor, method = 'square', diag = FALSE ,
         tl.pos = 'd')

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

#threshold
r = center_corr[order(center_corr[1:50,1:50], decreasing=TRUE)]
r
eta = r[cut_point]
eta

##network
center_corr[center_corr<eta]<-0
center_corr[center_corr>eta]<-1
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

 
membership(center_member) 
plot(center_member,network) 

adj.rand.index(membership(true_member),membership(center_member)) 
