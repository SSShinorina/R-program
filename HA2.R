############# TASK 1 ####################
data=load(Exam_2022)

# Load the dataset
data <- read.table(gzfile("zip.train"))
data <- as.matrix(data)
# Choose lucky number and want to choose 3.
Three <- data[which(data[,1]==3),2:257]
Three
#Display image with image()
colors <- c('white','black'); cus_col <- colorRampPalette(colors=colors)
# for 1st 24 cases
par(mfrow = c(2,4))
for (i in 1:24){
  z <- matrix(Three[i,256:1],16,16,byrow=T)[,16:1]
  z = image(t(z),col=cus_col(256))
}



################### TASK 2 #################
#Find covariance matrix for my lucky number data Four. We apply eigen() on Covariance matrix and then we find out the vectors from the function result.
  
Cov<-cov(Three)
Cov
 
result = eigen(Cov)
par(mfrow = c(2,4))

#displaying 1st 4 eigen vectors
for(i in 1:4){
z <- matrix(result$vectors[256:1,i],16,16,byrow=T)[,16:1]
z
image(t(z),col=cus_col(256))
}

#displaying last 4 eigen vectors
for(i in 13:16){
  z <- matrix(result$vectors[256:1,i],16,16,byrow=T)[,16:1]
  z
  image(t(z),col=cus_col(256))
}
######################## TASK 3  #########################
y=Three[ ,256:1]%*%result$vectors

#Approximate number 3 (i=1) with 30, 60, 100, 150 and 200 principal components respectively:
img1=matrix(1:7680,256,30)
for(i in 1:30){
  img1[ ,i]=y[1,i]*t(result$vectors[ ,i])
}
img1=rowSums(img1)

img2=matrix(1:15360,256,60)
for(i in 1:60){
  img2[ ,i]=y[1,i]*t(result$vectors[ ,i])
}
img2=rowSums(img2)

img3=matrix(1:25600,256,100)
for(i in 1:100){
  img3[ ,i]=y[1,i]*t(result$vectors[ ,i])
}
img3=rowSums(img3)

img4=matrix(1:38400,256,150)
for(i in 1:150){
  img4[ ,i]=y[1,i]*t(result$vectors[ ,i])
}
img4=rowSums(img4)

img5=matrix(1:51200,256,200)
for(i in 1:200){
  img5[ ,i]=y[1,i]*t(result$vectors[ ,i])
}
img5=rowSums(img5)

#sort the elements into 16x16 martices
z <- matrix(Three[1,256:1],16,16,byrow=T)[,16:1]  
z1 <- matrix(img1[1:256],16,16,byrow=T)[,16:1]
z2 <- matrix(img2[1:256],16,16,byrow=T)[,16:1]
z3 <- matrix(img3[1:256],16,16,byrow=T)[,16:1]
z4 <- matrix(img4[1:256],16,16,byrow=T)[,16:1]
z5 <- matrix(img5[1:256],16,16,byrow=T)[,16:1]

#Visualize the image approximation for respective number of principal
#the first one being the original 3 (case 1)
par(mfrow = c(2,3))

image(t(z1),col=cus_col(256))
image(t(z2),col=cus_col(256))
image(t(z3),col=cus_col(256))
image(t(z4),col=cus_col(256))
image(t(z5),col=cus_col(256))
image(t(z),col=cus_col(256))
 
#Find MSE for each pixel of lucky number dataset and estimated dataset.
# MSE of 1st 30 principal component and the original dataset
mse30 = sum((Three[1,256:1]-img1[1:256])^2)
mse30/256

# MSE of 1st 60 principal component and the original dataset
mse60 = sum((Three[1,256:1]-img2[1:256])^2)
mse60/256

# MSE of 1st 100 principal component and the original dataset
mse100 = sum((Three[1,256:1]-img3[1:256])^2)
mse100/256

# MSE of 1st 150 principal component and the original dataset
mse150 = sum((Three[1,256:1]-img4[1:256])^2)
mse150/256

# MSE of 1st 200 principal component and the original dataset
mse200 = sum((Three[1,256:1]-img5[1:256])^2)
mse200/256


#################################  TASK 4 #########################
 
#while loop to find the ratio: variability of different number of principal 
#components to the total variability.

#vector with cumulative sums of diagonal elements from the covariance matrix of 
#principal components:
diagCovY=cumsum(diag(cov(y))) 

##q is number of principal components
q=0
ratio=0

while(ratio<0.85){
  q=q+1
  ratio= diagCovY[q]/sum(diag(Cov))
}
#number of principal comp. and ratio right after reaching 85%:
q
ratio
#number of principal comp. and ratio just before reaching 85%:
q-1
ratio=diagCovY[q-1]/sum(diag(Cov))
ratio

##############################  TASK 5 #####################
data_56=data[which(data[,1]==5|data[,1]==6),1:257]
n = dim(data_56)[1]
id = sample(1:n, floor(0.8*n))  
tr_d = data_56[id, ] # training set
te_d = data_56[-id, ] # testing set

Cov = cov(tr_d)
Eigen = eigen(Cov)
Eigen_vector = Eigen$vectors
Mean = colMeans(tr_d)

#PCA on train data
train_PCA = (tr_d - Mean)%*%Eigen_vector[,1:2]

tr_d_5=subset(tr_d,tr_d[,1]==5)
train_PCA_5 = (tr_d_5 - Mean)%*%Eigen_vector[,1:2]

tr_d_6=subset(tr_d,tr_d[,1]==6)
train_PCA_6= (tr_d_6 - Mean)%*%Eigen_vector[,1:2]

#PCA on test data 
test_PCA = (te_d - Mean)%*%Eigen_vector[,1:2]  

te_d_5=subset(te_d,te_d[,1]==5)
test_PCA_5 = (te_d_5 - Mean)%*%Eigen_vector[,1:2]

te_d_6=subset(te_d,te_d[,1]==6)
test_PCA_6 = (te_d_6 - Mean)%*%Eigen_vector[,1:2]

mvpdf <- function(x, mu, sigma) {

    if (det(sigma) == 0) {
    warning("Determinant is equal to 0.")
  }
  apply(train_PCA, 1, function(x) exp(-(1/2) * (t(x) - mu) %*% MASS::ginv(sigma) %*% 
                                t(t(x) - mu))/sqrt(det(2 * pi * sigma)))
}

mu1= colMeans(train_PCA_5)
sigma1 = cov(train_PCA_5)

prob_5 <- mvpdf(test_PCA_5, mu1, sigma1)
prob_5_uniform <- sum(train_PCA_5)/nrow(train_PCA)
prob_5 <- prob_5*prob_5_uniform

mu2= colMeans(train_PCA_6)
sigma2 = cov(train_PCA_6)

prob_6 <- mvpdf(test_PCA_6, mu2, sigma2)
prob_6_uniform <- sum(train_PCA_6)/nrow(train_PCA)
prob_6 <- prob_6*prob_6_uniform


GMM = (1:100);
for (i in 1:100) {
  if (prob_5[i]>prob_6[i]){GMM[i]=5}
  else if (prob_5[i]<prob_6[i]){GMM[i]=6}
}
GMM

Test = (1:100);
for (i in 1:100) {
  if (test_PCA_5[i]>test_PCA_6[i]){Test[i]=5}
  else if (test_PCA_5[i]<test_PCA_6[i]){Test[i]=6}
}
Test

 
rightGuess = (1:100)
for (i in 1:100) {
  if (GMM[i]==Test[i]){rightGuess[i]="correct"}
  else if (GMM[i]!=Test[i]){rightGuess[i]="false"}
}
rightGuess
sum(rightGuess == "correct")/100
 
 