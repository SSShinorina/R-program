############### Task 1 ########
library(bootstrap)
library(MVN)
data(scor)
scor

## summary of score data ###### 
summary(scor)

#### using mvn for summarize
mvn(scor)
mardia_res = mvn(data=scor, mvnTest = 'mardia')
mardia_res$multivariateNormality

hz_result = mvn(data=scor, mvnTest = 'hz')
hz_result$multivariateNormality

royston_result = mvn(data = scor, mvnTest = "royston")
royston_result$multivariateNormality

doornik_hansen_result = mvn(data=scor, mvnTest = "dh")
doornik_hansen_result$multivariateNormality

energy_result = mvn(data=scor, mvnTest = "energy")
energy_result$multivariateNormality

plot = mvn(data = scor, multivariatePlot = "qq")


####### univariant ##########
uni_result_qq = mvn(data=scor, mvnTest = "royston", univariatePlot = "qqplot")
uni_result_qq = mvn(data=scor, mvnTest = "royston", univariatePlot = "histogram")

sw_uni_result = mvn(data=scor, univariateTest = "SW", mvnTest = "royston")
sw_uni_result$univariateNormality
sw_uni_result$Descriptives


#### remove "sta" from the model ######

sta_dataset = subset(scor, select=-(sta))
new_dataset = subset(sta_dataset, select=-(ana))
new_dataset

mardia_res = mvn(data=sta_dataset, mvnTest = 'mardia')
mardia_res$multivariateNormality

hz_result = mvn(data=sta_dataset, mvnTest = 'hz', multivariateOutlierMethod='quan')
hz_result = mvn(data = sta_dataset, mvnTest = 'hz', multivariateOutlierMethod = 'adj')
hz_result$multivariateNormality

royston_result = mvn(data = sta_dataset, mvnTest = "royston")
royston_result$multivariateNormality

doornik_hansen_result = mvn(data=sta_dataset, mvnTest = "dh")
doornik_hansen_result$multivariateNormality

energy_result = mvn(data=sta_dataset, mvnTest = "energy")
energy_result$multivariateNormality

plot = mvn(data = new_dataset, multivariatePlot = "qq", multivariateOutlierMethod = 'quan')
plot = mvn(data = new_dataset, multivariatePlot = "qq", multivariateOutlierMethod = 'adj')


#### now the data set has mec,veg and alg. which are significant. ########




#########################################################################

############### TASK 2  ###################

TrainM <- Train[Train$Diagnosis == "M", ]
TrainB <- Train[Train$Diagnosis =="B", ]

########## calculate mean and sigma for both train adtaset #################
meanBnum = subset (TrainB, select = -(Diagnosis))
meanB = colMeans(meanBnum)

meanMnum = subset (TrainM, select = -(Diagnosis))
meanM = colMeans(meanMnum)

##### covariance/sigma ######
covB=cov(subset(TrainB, select = -(Diagnosis)))
covM=cov(subset(TrainM, select = -(Diagnosis)))

###### gmm ######
mvpdf <- function(x, mu, sigma) {
  if (det(sigma) == 0) {
    warning("Determinant is equal to 0.")
  }
  apply(x, 1, function(x) exp(-(1/2) * (t(x) - mu) %*% MASS::ginv(sigma) %*% 
                                t(t(x) - mu))/sqrt(det(2 * pi * sigma)))
}
z <- mvpdf(meanMnum, meanM, covM)
z
