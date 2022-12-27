###### TASK 1 #########
library(bootstrap)
data(scor)
scor
summary(scor)
plot(scor$mec,scor$vec)


######### TASK 2 #########
n = 1000
sum = 0 
result = numeric(n)
for (i in 1:n){
  sum = sum+ rbinom(n,1,0.5)
  result[i] = sum/i
}
result
plot(result, type = "line")
abline(h=0.5, col ="green")

########## TASK 3 ##########
f = function(x){x^3 -x -1}
ff = function(x){3*x^2-1}
error = 10
xold = 1
while (error > 0.1){
  xnew = xold -(f(xold)/ff(xold))
  error= abs(xold - xnew)
  xold = xnew 
}
f(X)



############### TASK 4 #############
