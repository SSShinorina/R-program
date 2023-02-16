library("MASS")
library(Matrix)
library(glmnet)

#### Creating sample data
set.seed(898989)
sample_size = 100
sample_mean = 0

#sparse covariance matrix
sample_covariace_matrix = matrix(c(10,5), ncol=50)
sample_covariace_matrix = sparse.model.matrix(sample_covariace_matrix[0:50])
sample_covariace_matrix

sample_data = mvrnorm(n=sample_size, mu=0,Sigma=sample_covariace_matrix)
sample_data


#### sub experimental data
for (i in nrow(sample_data)){
  if(sample_data[i]<0){
   a = sample_data*0.5+sample_data
  }
  else{
    a = sample_data*0.5-sample_data
  }
}
a

df1= data.frame(sample_data)
df2 = data.frame(a)

sub_data = cbind(df1, df2)
 

### centering
center_scale <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

sub_center_data=center_scale(sub_data)
sub_center_data

## Correlation

center_corr = cor(sub_center_data,method="pearson")
center_corr
plot(center_corr)

corrplot(cor(sub_center_data),
         method = "number",
         type = "upper"
)


##### clustering
clusters = kmeans(sub_center_data,4)
clusters



###################################################
######## without centering ######
wo_center_corr = cor(sub_data,method="pearson")
wo_center_corr
plot(wo_center_corr)

corrplot(cor(sub_data),
         method = "number",
         type = "upper"
)

##### clustering
clusters = kmeans(sub_data,4)
clusters
