rm(list = ls())

#--- generate the data ---#
DGP_ellipse <- function(N = 50, seed = 8312){
  set.seed(seed)
  oval_fun <- function(x,a=1,b=0.5){b*sqrt(1-(x/a)^2)}
  x11 = runif(N, -1, 1)
  x12 = c(oval_fun(x11[1:(.5*N)]),-oval_fun(x11[(.5*N+1):N])) + rnorm(N, 0, 0.05)
  X = cbind(x11, x12)
  x21 = runif(N, -1.5, 1.5)
  x22 = c(oval_fun(x21[1:(.5*N)],a=1.5,b=0.75),-oval_fun(x21[(.5*N+1):N],a=1.5,b=0.75)) + rnorm(N, 0, 0.05)
  X = rbind(X, cbind(x21,x22))
  Q = eigen(matrix(c(1,-4,-4,1),2,2))$vectors
  X = X%*%Q
  y = c(rep(1,N), rep(0, N))
  d = cbind(y, X)
  return(d)
}
N = 10
d = DGP_ellipse(N)
y = d[,1]
X = d[,-1]

# visualize
plot(X, pch=20, col = y+2, xlab = "X1", ylab = "X2", asp = 1, cex = 3)
#--- generate the data OVER ---#

#--- tr_te_split ---#
id = sample(1:(2*N), N*0.2)
X_tr = X[-id, ]
X_te = X[id, ]
y_tr = y[-id]
y_te = y[id]
#--- tr_te_split OVER ---#

#### training
k_fun = function(x,y){ 
  return((sum(x*y))^2)
}

## calculate the gram matrix
n=dim(X_tr)[1]
K = matrix(0, n, n)
for(i in 1:n){
  for(j in i:n){
    K[i,j] = k_fun(X_tr[i,],X_tr[j,])
  }
}
K = K + t(K) - diag(diag(K))

## calculate the centralized gram matrix
C = matrix(1/n, n, n)
K = K-C%*%K-K%*%C+C%*%K%*%C
## eigen decomposition
res = eigen(K)
## calculate the principle components
zz = res$vectors
plot(zz[,3], rep(0,n), pch=20, col = y+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)
zz[3]

#### testing
k_fun = function(x,y){ 
  return((sum(x*y))^2)
}

## calculate the gram matrix
n=dim(X_te)[1]
K = matrix(0, n, n)
for(i in 1:n){
  for(j in i:n){
    K[i,j] = k_fun(X_te[i,],X_te[j,])
  }
}
K = K + t(K) - diag(diag(K))

## calculate the centralized gram matrix
C = matrix(1/n, n, n)
K = K-C%*%K-K%*%C+C%*%K%*%C
## eigen decomposition
res = eigen(K)
## calculate the principle components
zz_test = res$vectors
zz_test[3]

plot(zz[3],zz_test[3])
