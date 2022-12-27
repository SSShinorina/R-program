#### Task 1 #########
set.seed(2022)
N <- 100
x1 <- runif(N,1,2)
x2 <- runif(N,1,2)
X <- cbind(x1,x2)
Y <- ifelse(x2>-.6*x1+2.35,-1,1)
y <- as.factor(Y)
data = data.frame(x1=x1, x2=x2, y=y)
str(data)
library(ggplot2)
ggplot(data, aes(x = x1, y = x2)) + 
  geom_point(aes(colour=y, shape=y), size = 3) +
  xlab("X1") + 
  ylab("X2") + 
  ggtitle("y vs x1 and x2")
perceptron <- function(x, y, eta, n.iter) {
  
  # initialize weight vector
  #weight <- rep(0, dim(x)[2] + 1)
  weight <- runif(dim(x)[2] + 1)
  errors <- rep(0, n.iter)
  
  
  # loop over number of epochs n.iter
  for (j in 1:n.iter) {
    
    # loop through training data set
    for (i in 1:length(y)) {
      
      # Predict binary label using Heaviside activation function
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[i, ])) + weight[1]
      if(z < 0) {
        ypred <- -1
      } else {
        ypred <- 1
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weightdiff <- eta * (y[i] - ypred) * 
        c(1, as.numeric(x[i, ]))
      weight <- weight + weightdiff
      
      # Update error function
      if ((y[i] - ypred) != 0.0) {
        errors[j] <- errors[j] + 1
      }
      
    }
  }
  
  # final weight to classify a new object
  print(weight)
  return(list(errors=errors, w= weight))
}

err <- perceptron(X[1:70,], Y[1:70], 1, 70)
err
err$w
X = cbind(rep(1, dim(X[71:100,])[1]),X[71:100,] )
w = as.matrix(err$w)
predict = ifelse(X%*%w>0, 1, -1)
predict-Y[71:100]


######################################################
#### TASK 2.1 ######
# we are using cv.glmnet build-in function to train the random values. We summarize the model,
# estimate the accuracy and kappa statistic.
library(glmnet)
library(MASS); library(caret)
library(psych)
hwd = data.frame(data)

#split dataset into train and test
set.seed(8321)
id_train <- sample(1:dim(hwd)[1], 0.8*dim(hwd)[1])
data_train <- hwd[id_train, ]
data_test <- hwd[-id_train, ]

#apply cv.glmnet()  
cv.out <- cv.glmnet(x = as.matrix(data_train),
                     y = as.factor(data_train$V1),
                     family = "multinomial",
                     type.measure = "class",
                     nfolds = 10)
cv.out
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
 
lasso.pred = predict(cv.out ,as.matrix(data_test),s= bestlam , type="class")
lasso.pred
confusionMatrix(as.factor(lasso.pred), as.factor(data_test$V1))

## kappa statistics 
cohen.kappa(x=cbind(as.matrix(lasso.pred),as.factor(data_test$V1)))


############# TASK 2.2 #############

colors <- c('white','black'); cus_col <- colorRampPalette(colors=colors)
set.seed(8321)
par(mfrow = c(2,3))
 
for (i in sample(1:dim(lasso.pred)[1], 25)){
  z <- matrix(as.numeric(data_test[i,257:2]),16,16,byrow = T)[,16:1]
  image(t(z),col=cus_col(256))
  title(sprintf("prediction: %s", lasso.pred[i]))
}



##########################################################################################
##################### TASK 3.1 ######################
# derive complex equation to calculate kth kernel principal component for the new observation

## define the kernel function 
k_fun = function(x,y){ 
  return((sum(x*y))^2)
}
## calculate the gram matrix
K = matrix(0, n, n)
for(i in 1:n){
  for(j in i:n){
    K[i,j] = k_fun(X[i,],Xnew[j,])
  }
}
K = K + t(K) - diag(diag(K))

## calculate the centralized gram matrix
C = matrix(1/n, n, n)
K = K-C%*%K-K%*%C+C%*%K%*%C

## calculate principal component for Xnew
lambdak = K.eigen$values
miuk = K.eigen$vector
pc = 1/lambdak*t(miuk)*(Xnew - (1/n*K*n))


############# TASK 3.2 #############
##source("Task3_KPCA.R")

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
N=10
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
K_centerd = K-C%*%K-K%*%C+C%*%K%*%C
## eigen decomposition
res = eigen(K_centerd)
## calculate the principle components
zz = res$vectors
plot(zz[,3], rep(0,n), pch=20, col = y_tr+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)


#### testing
k_fun = function(x,y){ 
  return((sum(x*y))^2)
}

## calculate the gram matrix
n_te=dim(X_te)[1]
K = matrix(0, n_te, n_te)
for(i in 1:n_te){
  for(j in i:n_te){
    K[i,j] = k_fun(X_te[i,],X_te[j,])
  }
}
K = K + t(K) - diag(diag(K))

## calculate the centralized gram matrix
C = matrix(1/n_te, n_te, n_te)
K_center = K-C%*%K-K%*%C+C%*%K%*%C
## eigen decomposition
res = eigen(K_center)
## calculate 3rd the principle components
lambdak = res$values
miuk = res$vector

I= diag(K)

pc = 1/(lambdak)*t(miuk)*(I-C)*((k_fun(X_tr[i,],3))-(1/n_te)*K*1)

plot(NULL, ylab="y label", xlab="x label",
     xlim=c(-1, 1), ylim=c(-1, 1))
points(zz[,3], rep(0,n), pch=20, col = y_tr+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)
points(pc[,1],rep(0,n_te), pch=20, col = y_te+1, xlab = "Z2", ylab = "", asp = 1, cex = 3) 
points(pc[,2],rep(0,n_te), pch=20, col = y_te+1, xlab = "Z2", ylab = "", asp = 1, cex = 3)
 

##########################################################################################
########### TASK 5.1 #################

library(kernlab)
set.seed(8321)
hwd = data.frame(data)

#split dataset into train and test
index = sample(1:nrow(hwd), 0.8*nrow(hwd))
train = hwd[index,]
dim(train)
test = hwd[-index,]
dim(test)

# SVM with vanilladot
(svm_1 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'vanilladot', kpar=list(), C = 0.0001))
# Training error : 0.142667 
(svm_2 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'vanilladot', kpar=list(), C = 0.0005))
#Training error : 0.061  
(svm_3 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'vanilladot', kpar=list(), C = 0.001))
#Training error : 0.048
(svm_4 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'vanilladot', kpar=list(), C = 0.005))
#Training error : 0.02 
(svm_5 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'vanilladot', kpar=list(), C = 0.1))
#Training error : 0
(svm_6 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'vanilladot', kpar=list(), C = 1))
#Training error : 0
 
# The optimal C is 0.0001 which is small.
# svm_1 is the final model.
# estimate the performance of testing dataset
pred_test<- predict(svm_1, test)
accuracy = mean(pred_test==test$V1)
accuracy


#### TASK 5.2 ######
# use rbfdot kernel function

(rbf_1 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'rbfdot', kpar=list(sigma = 0.0001), cross = 10))
#Cross validation error : 0.111333 
(rbf_2 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'rbfdot', kpar=list(sigma = 0.0005), cross = 10))
#Cross validation error : 0.062333 
(rbf_3 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'rbfdot', kpar=list(sigma = 0.001), cross = 10))
#Cross validation error : 0.053  
(rbf_4 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'rbfdot', kpar=list(sigma = 0.005), cross = 10))
#Cross validation error : 0.049333
(rbf_5 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'rbfdot', kpar=list(sigma = 0.1), cross = 10))
#Cross validation error : 0.718333 
(rbf_6 <- ksvm(as.factor(V1)~., data = hwd, kernel = 'rbfdot', kpar=list(sigma = 1), cross = 10))
#Cross validation error : 0.817667

#We want to choose 4th model where the sigma value is 0.005. Here cross validation error is lowest 
#than others which is 0.047333
# Final model is rbf_4 and measure performance of it according to testing set.
pred_test_rbf<- predict(rbf_4, test)
accuracy_rbf = mean(pred_test==test$V1)
accuracy_rbf

# accuracy are same for both kernel and it is 0.8533333.


####################################################
####### Task 6.1 ######
 
# Load packages
library(tidyverse) # visualization, model, transform, tidy and import data
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

train <- read_csv("C:/Users/mohgho/Documents/AlavRProgramming/titan_train.csv")
test <- read_csv("C:/Users/mohgho/Documents/AlavRProgramming/titan_test.csv")
full  <- bind_rows(train, test) # bind training & test data
# check data
str(full)
dim(full)
 
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Show title counts by sex
table(full$Sex, full$Title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
# Show title counts by sex again
table(full$Sex, full$Title)
# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])
 
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')
 
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

 
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', 
           shade=TRUE)
 
# This variable appears to have a lot of missing values
full$Cabin[1:28]
# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]
# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
 
# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']
 
# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)
# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
 
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'
 
# Show row 1044
full[1044, ]
 
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
 
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, 
                          na.rm = TRUE)
 
# Show number of missing Age values
sum(is.na(full$Age))
 
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[,!names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
# Save the complete output 
mice_output <- complete(mice_mod)
 
# Replace Age variable from the mice model.
full$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(full$Age))
 
## Feature Engineering: Round 2
 
# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()
# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
# Show counts
table(full$Child, full$Survived)
 
# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 
              'Miss'] <- 'Mother'
# Show counts
table(full$Mother, full$Survived)
# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)
 
md.pattern(full)
 

#### task 6.2
 
str(full)
head(full)
names(full)
full.imp = full[,!names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Deck','Title')]
head(full.imp)
 
library(mice)

fullData_imp = mice(full.imp, m=5, maxit=100,
method = "pmm", seed=500, printFlag = FALSE)
str(fullData_imp$educ)

fullData_imp_new <- mice::complete(fullData_imp, 5)
## earnings_imp_new = mice::as.mids(earnings_imp_new)
fullData_imp_new %>% summary()
 
# Loading package
library(caTools)
library(randomForest)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(fullData_imp_new), replace=TRUE, prob=c(0.7,0.3))

train  <- fullData_imp_new[sample, ]
test   <- fullData_imp_new[!sample, ]



# Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[-1],
                             y = train$Survived, ntree = 500)



classifier_RF %>%  head()

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-1])

# Confusion Matrix
confusion_mtx = table(test[, 5], y_pred)
confusion_mtx %>% head()

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
