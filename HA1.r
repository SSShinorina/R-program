install.packages("rmarkdown")
# load MVN package
install.packages("MVN")
library(MVN)
library(bootstrap)
data(scor)

###########################
#summary of the scor data, and chacking for normality
summary(scor)
#using MVN, multivariate normality test. 
#to check the data set ’scor’ with normality assumption and potential outliers

#Mardia's MVN test to test if the data follows a multivariate normal distribution
# null hypothesis, the data comes from a multivariate normal distribution
mardia_res <- mvn(data = scor, mvnTest= "mardia")
mardia_res$multivariateNormality
#we observe a greater p-stat than 0.05 on kurtosis but not scewness, hence we can't say we have data multivarnorm dist
#NO - on skewness

#Using henze-zirkler test to see if the data is multivariate normally distributed
hz_res <- mvn(data = scor, mvnTest= "hz")
hz_res$multivariateNormality
#according to the hz-test the data is not multiovariate normally distributed on a 0.05 confidence level
#NO

#Using royston test to see if the data is multivariate normally distributed
royston_res <- mvn(data = scor, mvnTest= "royston")
royston_res$multivariateNormality
#on a 95%confidence lvl the data is not multivariate normally distributed according to the roystons MVN test
#NO

#Using doornik-hansen test to see if the data is multivariate normally distributed
dh_res <- mvn(data = scor, mvnTest= "dh")
dh_res$multivariateNormality
#NO

#Using  test to see if the data is multivariate normally distributed
energy_res <- mvn(data = scor, mvnTest= "energy")
energy_res$multivariateNormality
#NO

#assumably the data doesntr follow multivariate normal distribution.
##################################
qq <- mvn(data = scor, multivariatePlot = "qq")
#from the normality test and qq-plot we can say that the data doesn't satisfy MVN

###################################
#univariate distributions and plots of the variables to observe the reason for MVN deviation.

#univariate qq-plots
resultuniv <- mvn(data = scor, mvnTest = "royston", univariatePlot = "qqplot")

#univariate histograms
resulthist <- mvn(data = scor, mvnTest = "royston", univariatePlot = "histogram")

#we see that "vec" "alg" follows MVN
#our issues w/ MVN follows from the scewness of "mec", "ana" and "sta"

#####################################

resultunivariate <- mvn(data = scor, mvnTest = "royston", univariateTest = "SW", desc = TRUE)
resultunivariate$univariateNormality
#we observe that the "ana" and "sta" is noit following the MVN hence we drop these one-by-one drom the data set

#####################################
#data set without "ana" 
scor_new <- subset(scor, select = -(ana))

#MVN testing, data set
mardia_res <- mvn(data = scor_new, mvnTest= "mardia")
mardia_res$multivariateNormality

hz_res <- mvn(data = scor_new, mvnTest= "hz")
hz_res$multivariateNormality

royston_res <- mvn(data = scor_new, mvnTest= "royston")
royston_res$multivariateNormality

dh_res <- mvn(data = scor_new, mvnTest= "dh")
dh_res$multivariateNormality

energy_res <- mvn(data = scor_new, mvnTest= "energy")
energy_res$multivariateNormality
#not MVN, we need to remove both variables
##########################################
##########################################
#data set without "ana" and "sta and "mec" 
scor_new <- subset(scor, select = -(ana))
scor_new <- subset(scor_new, select = -(sta))

#MVN testing, data set
mardia_res <- mvn(data = scor_new, mvnTest= "mardia")
mardia_res$multivariateNormality

hz_res <- mvn(data = scor_new, mvnTest= "hz")
hz_res$multivariateNormality

royston_res <- mvn(data = scor_new, mvnTest= "royston")
royston_res$multivariateNormality

dh_res <- mvn(data = scor_new, mvnTest= "dh")
dh_res$multivariateNormality

energy_res <- mvn(data = scor_new, mvnTest= "energy")
energy_res$multivariateNormality

#We have MVN for the dataset on all tests except the Mardia skewness, lets observe the qq-plot to see normality
qq <- mvn(data = scor_new, multivariatePlot = "qq")
qq
#The best data set is including 3 variables but we see issues with potential outliers
############################################

# Mahalanobis distance
result <- mvn(data = scor_new, mvnTest = "hz", multivariateOutlierMethod = "quan")
result
# Adjusted Mahalanobis distance
result <- mvn(data = scor_new, mvnTest = "hz", multivariateOutlierMethod = "adj")


################################################################################
#Task 2
################################################################################
# Multivariate Normal PDF Function Given a matrix x, mu (mean), and
# sigma (covariance), we can calculate the probability density for each
# row using the apply function. The function returns a column vector of
# probabilities.

#we want to find GMM prob per class (cancer/not cancer) and then compare to the test data 
#to get var and mean we want to use MLE with log likelihood 
#we want to build 2 gaussian models

#we need to sum up the log likelihood for each data point.

#sum log(gaussian dist.)
#assume that gaussians have a diagonal covariance matrix
#therefore tyhe covariance matrix lambda gets replaced with the diagonal variance vector sigma^2

#split data frame based on particular column value
Train$Diagnosis <- as.factor(Train$Diagnosis)
TrainM <- Train[Train$Diagnosis == "M",]
TrainB <- Train[Train$Diagnosis == "B", ]

#mu function, array w/ means of each variable
meanBnum <- subset(TrainB, select = -(Diagnosis))
meanB <- colMeans(meanBnum)

meanMnum <- subset(TrainM, select = -(Diagnosis))
meanM <- colMeans(meanMnum)

meanB
meanM
#Another function for meanapply(d, 2, mean)

#Covariance matrix

covBnum <- cov(subset(TrainB, select = -(Diagnosis)))
covMnum <- cov(subset(TrainM, select = -(Diagnosis)))
 

#GMM to get probability 

#generating GMM
mvpdf <- function(x, mu, sigma) {
  if (det(sigma) == 0) {
    warning("Determinant is equal to 0.")
  }
  apply(x, 1, function(x) exp(-(1/2) * (t(x) - mu) %*% MASS::ginv(sigma) %*% 
                                t(t(x) - mu))/sqrt(det(2 * pi * sigma)))
}
sigma
#importing test data and removing the diagnosis
testData <- subset(Test, select = -(Diagnosis))

#testing the test data on B
probB <- mvpdf(testData, meanB, covBnum)
probBuniform <- sum(Train$Diagnosis =="B")/nrow(Train)
probB <- probB*probBuniform
probB
#testing the test data on M
probM <- mvpdf(testData, meanM, covMnum)
probMuniform <- sum(Train$Diagnosis =="M")/nrow(Train)
probM <- probM*probMuniform

GMM = (1:100);
for (i in 1:100) {
    if (probB[i]>probM[i]){GMM[i]="B"}
    else if (probB[i]<probM[i]){GMM[i]="M"}
}
GMM
TestCorrect <- Test$Diagnosis
rightGuess = (1:100)
for (i in 1:100) {
  if (GMM[i]==TestCorrect[i]){rightGuess[i]="correct"}
  else if (GMM[i]!=TestCorrect[i]){rightGuess[i]="false"}
}
rightGuess
sum(rightGuess == "correct")/100



#############################################################
############## Task 3.1 #################
set.seed(8312)
library(mixtools)
Sigma <- matrix(c(1,sqrt(1*4)*0.7,sqrt(1*4)*0.7,4),2,2)
data3 = rmvnorm(1000, c(2,3), Sigma)
data3
###head(data3)
####cor(data3)
plot(data3)


########### TASK 3.2 ##############
library(Rlab)
set.seed(8231)
Sigma1 <- matrix(c(0.2,sqrt(0.2*0.6)*0.5,sqrt(0.2*0.6)*0.5,0.6),2)
data_latent_1 = rmvnorm(1000, c(2,3), Sigma1)
data_latent_1
Sigma2 <- matrix(c(0.4,sqrt(0.4*0.3)*0.5,sqrt(0.4*0.3)*0.5,0.3),2)
data_latent_2 = rmvnorm(1000,c(3,2),Sigma2)
data_latent_2
data_bernouli_1 = pbern(data_latent_1, p=0.3)
data_bernouli_1
data_bernouli_2 = pbern(data_latent_2, p=0.3)
data_bernouli_2


##################### TASK 3.3 ##############
set.seed(1000)
data_latent_all <- rbind(head(data_latent_1,500), head(data_latent_2,500))
data_latent_all
mu_latent <- list(c(2,3),c(3,2))
mu_latent
output <-mvnormalmixEM(data_latent_all, arbmean = TRUE, mu = mu_bernouli,epsilon = 1e-02)
output



####################### TASK 4 #######################
set.seed(1000)
#### initial lambda1 = 5, lambda2 = 7 and pi = 0.5
######### likelihood with poisson distribution for different lambda.######
x<-c(6,9,3,6,6,13,1,10,5,4)
P1 = dpois(x, 5)
P2 = dpois(x, 7)
P1
P2

####### Z1, Z2. Posterior probability for 2 different lambdas. ########## 
Z1 = (P1*0.5/(P1*0.5+P2*0.5))
Z1
Z2 = (P2*0.5/(P1*0.5+P2*0.5))
Z2

########### Find lambda1, lambda2 and pi.##########
 

for(i in 1:100){
  lambda_1 = (sum((x)*Z1))/5
  lambda_2 = (sum((x)*Z2))/5
  pi = sum(dpois(x,lambda_1+lambda_2))
}
lambda_1; lambda_2; pi


