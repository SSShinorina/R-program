#################### TASK 1 #########################
library(dplyr)
data <- Knee

surgery = data[data$Grupp == 1,]
non_surgery = data[data$Grupp == 2,]

#select all columns for surgery treatment group
sub_data = surgery %>% select(I_Q_conNm_weight:C_H_eccNm_weight)
sub_data <- data.frame(sub_data)
sub_data

(D <- cbind(as.numeric(sub_data[,1])-as.numeric(sub_data[,2]), as.numeric(sub_data[,3])-as.numeric(sub_data[,4]),
            as.numeric(sub_data[,5])-as.numeric(sub_data[,6]),as.numeric(sub_data[,7])-as.numeric(sub_data[,8])))
D <- data.frame(D)
colnames(D) <- c("I_Q", "C_Q", "I_H", "C_H")
D <- replace(D, is.na(D), 0)

n <- dim(D)[1]; p <- dim(D)[2]

# Hotellings T2-test
delta0 <- c(0,0)
(T2<-n*t(colMeans(D))%*%solve(cov(D))%*%colMeans(D))
(cv <- ((n-1)*p)/(n-p)*qf(0.95, p, n-p))

# Check T² confidence interval
c(colMeans(D)[1] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n), 
  colMeans(D)[1] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n))
c(colMeans(D)[2] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n), 
  colMeans(D)[2] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n))
c(colMeans(D)[3] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[3,3]/n), 
  colMeans(D)[3] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[3,3]/n))
c(colMeans(D)[4] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[4,4]/n), 
  colMeans(D)[4] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[4,4]/n))

# Bonferroni correction CI
c(colMeans(D)[1] - qt(0.9875, n-1)*sqrt(cov(D)[1,1]/n), 
  colMeans(D)[1] + qt(0.9875, n-1)*sqrt(cov(D)[1,1]/n))
c(colMeans(D)[2] - qt(0.9875, n-1)*sqrt(cov(D)[2,2]/n), 
  colMeans(D)[2] + qt(0.9875, n-1)*sqrt(cov(D)[2,2]/n))
c(colMeans(D)[3] - qt(0.9875, n-1)*sqrt(cov(D)[3,3]/n), 
  colMeans(D)[3] + qt(0.9875, n-1)*sqrt(cov(D)[3,3]/n))
c(colMeans(D)[4] - qt(0.9875, n-1)*sqrt(cov(D)[4,4]/n), 
  colMeans(D)[4] + qt(0.9875, n-1)*sqrt(cov(D)[4,4]/n))


#select columns for non_surgical treatment group and do apir wise comparision
sub_data = non_surgery %>% select(I_Q_conNm_weight:C_H_eccNm_weight)
sub_data <- data.frame(sub_data)
sub_data

(D <- cbind(as.numeric(sub_data[,1])-as.numeric(sub_data[,2]), as.numeric(sub_data[,3])-as.numeric(sub_data[,4]),
            as.numeric(sub_data[,5])-as.numeric(sub_data[,6]),as.numeric(sub_data[,7])-as.numeric(sub_data[,8])))
D <- data.frame(D)
colnames(D) <- c("I_Q", "C_Q", "I_H", "C_H")
D <- replace(D, is.na(D), 0)

n <- dim(D)[1]; p <- dim(D)[2]
 

# Hotellings T2-test
delta0 <- c(0,0)
(T2<-n*t(colMeans(D))%*%solve(cov(D))%*%colMeans(D))
(cv <- ((n-1)*p)/(n-p)*qf(0.95, p, n-p))

# Check T² confidence interval
c(colMeans(D)[1] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n), 
  colMeans(D)[1] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[1,1]/n))
c(colMeans(D)[2] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n), 
  colMeans(D)[2] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[2,2]/n))
c(colMeans(D)[3] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[3,3]/n), 
  colMeans(D)[3] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[3,3]/n))
c(colMeans(D)[4] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[4,4]/n), 
  colMeans(D)[4] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D)[4,4]/n))

# Bonferroni correction CI
c(colMeans(D)[1] - qt(0.9875, n-1)*sqrt(cov(D)[1,1]/n), 
  colMeans(D)[1] + qt(0.9875, n-1)*sqrt(cov(D)[1,1]/n))
c(colMeans(D)[2] - qt(0.9875, n-1)*sqrt(cov(D)[2,2]/n), 
  colMeans(D)[2] + qt(0.9875, n-1)*sqrt(cov(D)[2,2]/n))
c(colMeans(D)[3] - qt(0.9875, n-1)*sqrt(cov(D)[3,3]/n), 
  colMeans(D)[3] + qt(0.9875, n-1)*sqrt(cov(D)[3,3]/n))
c(colMeans(D)[4] - qt(0.9875, n-1)*sqrt(cov(D)[4,4]/n), 
  colMeans(D)[4] + qt(0.9875, n-1)*sqrt(cov(D)[4,4]/n))





########################### TASK 2 ###############################
non_injured = data[data$Grupp == 3,]
data_2 = non_injured %>% select(C_D_Length1:C_D_Length3)
data_2

D2 = cbind(as.numeric(data_2$C_D_Length1), as.numeric(data_2$C_D_Length2), 
           as.numeric(data_2$C_D_Length3))
D2

D2 <- replace(D2, is.na(D2), 0)

colMeans(D2)

#Contrast matrix:
(C <- matrix(c(
              1,-1,0,
              1,0,-1), 2, 3, byrow = T))

C%*%colMeans(D2)

(n <- dim(D2)[1])

(q <- dim(D2)[2])

(T2 <- n*t(colMeans(D2))%*%t(C)%*%solve(C%*%cov(D2)%*%t(C))%*%C%*%colMeans(D2))

(cv <- qf(0.95, (q-1), (n-q+1)) * (n-1)*(q-1)/(n-q+1))

T2 > cv # Reject the null hypothesis

# Check significant differences
# Check T² confidence interval
c(colMeans(D2)[1] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D2)[1,1]/n), 
  colMeans(D2)[1] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D2)[1,1]/n))
c(colMeans(D2)[2] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D2)[2,2]/n), 
  colMeans(D2)[2] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D2)[2,2]/n))
c(colMeans(D2)[3] - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D2)[3,3]/n), 
  colMeans(D2)[3] + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D2)[3,3]/n))

# Bonferroni correction CI
c(colMeans(D2)[1] - qt(0.9875, n-1)*sqrt(cov(D2)[1,1]/n), 
  colMeans(D2)[1] + qt(0.9875, n-1)*sqrt(cov(D2)[1,1]/n))
c(colMeans(D2)[2] - qt(0.9875, n-1)*sqrt(cov(D2)[2,2]/n), 
  colMeans(D2)[2] + qt(0.9875, n-1)*sqrt(cov(D2)[2,2]/n))
c(colMeans(D2)[3] - qt(0.9875, n-1)*sqrt(cov(D2)[3,3]/n), 
  colMeans(D2)[3] + qt(0.9875, n-1)*sqrt(cov(D2)[3,3]/n))






########################### TASK 3 #############################
# dataset for surgery
data3 = surgery %>% select(I_Q_conNm_weight,I_Q_eccNm_weight,I_H_conNm_weight, I_H_eccNm_weight)
data3

data3 <- data.frame(data3)
data3

(D3_surgery <- cbind(as.numeric(data3[,1])-as.numeric(data3[,2]),as.numeric(data3[,3])-as.numeric(data3[,4])))
D3_surgery <- replace(D3_surgery, is.na(D3_surgery), 0)
D3_surgery

#dataset for non_surgery
data_non_surgery = non_surgery %>% select(I_Q_conNm_weight,I_Q_eccNm_weight,I_H_conNm_weight, I_H_eccNm_weight)
data_non_surgery

data_non_surgery <- data.frame(data_non_surgery)
data_non_surgery

(D3_non_surgery <- cbind(as.numeric(data_non_surgery[,1])-as.numeric(data_non_surgery[,2]),
                         as.numeric(data_non_surgery[,3])-as.numeric(data_non_surgery[,4])))
D3_non_surgery <- replace(D3_non_surgery, is.na(D3_non_surgery), 0)
D3_non_surgery

Cov_surgery = cov(D3_surgery)
Cov_non_surgery = cov(D3_non_surgery)

colnames(D3_surgery) <- c("I_Q","I_H")
colnames(D3_non_surgery) <- c("I_Q","I_H")

n1 <- dim(D3_surgery)[1]; p1 <- dim(D3_surgery)[2]
n2 <- dim(D3_non_surgery)[1]; p2 <- dim(D3_non_surgery)[2]
p=p1=p2
 
Spooled = ((n1-1)/(n1+n2-2))*Cov_surgery + ((n2-1)/(n1+n2-2))*Cov_non_surgery
Spooled

# Hotellings T2-test
colMeans(D3_surgery)
colMeans(D3_non_surgery)
sigma = colMeans(D3_surgery)-colMeans(D3_non_surgery)
X1 = mean(D3_surgery)
X2 = mean(D3_non_surgery)

delta0 <- c(0,0)
(T2<-t(X1-X2-sigma)%*%solve(((1/n1)+(1/n2))*Spooled)%*%(X1-X2-sigma))
(cv <- (((n1+n2-2)*p)/(n1+n2-p-1))*pf(0.05,p,n1+n2-p-1))

T2 > cv # Reject null hypothesis

D3_surgery = data.frame(D3_surgery)
D3_non_surgery = data.frame(D3_non_surgery)

# Check T² confidence interval
c(colMeans(D3_surgery[1]) - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_surgery)[1,1]/n), 
  colMeans(D3_surgery[1]) + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_surgery)[1,1]/n))
c(colMeans(D3_non_surgery[1])- sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_non_surgery)[1,1]/n), 
  colMeans(D3_non_surgery[1]) + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_non_surgery)[1,1]/n))


c(colMeans(D3_surgery[2]) - sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_surgery)[2,2]/n), 
  colMeans(D3_surgery[2]) + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_surgery)[2,2]/n))
c(colMeans(D3_non_surgery[2])- sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_non_surgery)[2,2]/n), 
  colMeans(D3_non_surgery[2]) + sqrt(p*(n-1)/(n-p)*qf(0.95, p, n-p)*cov(D3_non_surgery)[2,2]/n))


# Bonferroni correction CI
c(colMeans(D3_surgery[1]) - qt(0.9875, n-1)*sqrt(cov(D3_surgery)[1,1]/n), 
  colMeans(D3_surgery[1]) + qt(0.9875, n-1)*sqrt(cov(D3_surgery)[1,1]/n))
c(colMeans(D3_non_surgery[1]) - qt(0.9875, n-1)*sqrt(cov(D3_non_surgery)[1,1]/n), 
  colMeans(D3_non_surgery[1]) + qt(0.9875, n-1)*sqrt(cov(D3_non_surgery)[1,1]/n))


c(colMeans(D3_surgery[2]) - qt(0.9875, n-1)*sqrt(cov(D3_non_surgery)[2,2]/n), 
  colMeans(D3_surgery[2]) + qt(0.9875, n-1)*sqrt(cov(D3_surgery)[2,2]/n))
c(colMeans(D3_non_surgery[2]) - qt(0.9875, n-1)*sqrt(cov(D3_non_surgery)[2,2]/n), 
  colMeans(D3_non_surgery[2]) + qt(0.9875, n-1)*sqrt(cov(D3_non_surgery)[2,2]/n))



###########################  TASK 4 ######################

#2 way MANOVA
#full model
D4 = data %>% select(I_Q_conNm_weight,I_Q_eccNm_weight,I_H_conNm_weight,
                     I_H_eccNm_weight, Gender, Grupp)
full <- manova(cbind(as.numeric(I_Q_conNm_weight),as.numeric(I_Q_eccNm_weight),
                    as.numeric(I_H_conNm_weight),as.numeric(I_H_eccNm_weight))
              ~ as.numeric(Gender) * as.numeric(Grupp), data = D4)
full <- replace(full, is.na(full), 0)
summary(full, test="Wilks")

#residue model
res <- manova(cbind(as.numeric(I_Q_conNm_weight),as.numeric(I_Q_eccNm_weight),
                    as.numeric(I_H_conNm_weight),as.numeric(I_H_eccNm_weight))
              ~ as.numeric(Gender) + as.numeric(Grupp), data = D4)
res <- replace(res, is.na(res), 0)
summary(res, test="Wilks")

#interaction is not significant. The factors are significant individually. Doing univariate analysis.
res = aov(D4$Grupp~D4$Gender)
summary(res)
res = aov(D4$Gender~D4$Grupp)
summary(res)

