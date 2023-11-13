library(faraway)


data<-faraway::fat
data<-data[,c(-2,-3,-8)]
ncol(data)
nrow(data)
for (i in 2:ncol(data)){
  data[,i]<-data[,i]-mean(data[,i])
  data[,i]<-data[,i]/sqrt(sum((data[,i]-mean(data[,i]))^2))
}
set.seed(123)
deigma<-sample(1:nrow(data),round(4/5*nrow(data)),replace = FALSE)
length(deigma)
training_sample<-data[deigma,]
test_sample<-data[-deigma,]

X<-training_sample[,-1]
Y<-training_sample$brozek
x_test<-test_sample[,-1]
y_test<-test_sample[,1]

library(glmnet)
lasso1<-glmnet(X,Y)
plot(lasso1,label = T)
plot(lasso1,xvar = 'lambda',label = T)

#--------------------------------------

X_matrix<-as.matrix(X) 
lasso2<-cv.glmnet(X_matrix,Y)
lasso2$lambda.min
lasso2$lambda.1se   

M1<-coef(lasso2,s="lambda.min") #find the coefficients of the model
M2<-coef(lasso2,s="lambda.1se")
M1.ind<-c(1,3,5,7,8,13,14) #store the indices to fit the model
M2.ind<-c(1,3,7,14)
model1<-lm(Y~.,data = X[,M1.ind]) #fit the models
model2<-lm(Y~.,data = X[,M2.ind])
MSE.M1<-sum((predict(model1,x_test[,M1.ind])-y_test)^2)/length(y_test)
MSE.M2<-sum((predict(model2,x_test[,M2.ind])-y_test)^2)/length(y_test)
M_Lasso<-model2
M1
M2
#-------------------------------------------------
x<-data[,-1]
y<-data[,1]
M3<-lm(y~.,data=x[,M2.ind])
M3
#-----------------------------------------
brozek<-data$brozek
age<-x$age
weight<-x$weight
height<-x$height
adipos<-x$adipos
neck<-x$neck
chest<-x$chest
abdom<-x$abdom
hip<-x$hip
thigh<-x$thigh
knee<-x$knee
ankle<-x$ankle
biceps<-x$biceps
forearm<-x$forearm
wrist<-x$wrist


predictors<-colnames(x)
response<-"brozek"
best_bic <- Inf
best_model <- NULL

# Loop over all subset sizes
for (k in 1:length(predictors)) {
  # Loop over all subsets of size k
  subsets <- combn(predictors, k, simplify = FALSE)
  for (subset in subsets) {
    # Construct the formula for this subset
    predictors_str <- paste(subset, collapse = "+")
    formula_str <- paste(response, "~", predictors_str)
    formula <- as.formula(formula_str)
    # Fit the model and compute its BIC
    model <- lm(formula, data = x)
    bic <- BIC(model)
    
    
    # If this is the best model so far, remember it
    if (bic < best_bic) {
      best_bic <- bic
      M4 <- model
    }
  }
}

# Print the best model and its BIC
summary(M4)
print(best_bic)
# will perform an exhaustive search over all subsets of predictors, fit a linear regression model for each subset, and then select the model with the lowest BIC. Note that this will be very slow if you have many predictors, because the number of subsets is 2^n where n is the number of predictors.

#-------------------------------------------------------------

n<-length(y)
set.seed(155)
randomise<-sample(1:n,n)
F1<-randomise[1:50]
F2<-randomise[51:100]
F3<-randomise[101:150]
F4<-randomise[151:200]
F5<-randomise[201:252]
folds<-list(F1,F2,F3,F4,F5)
M3.ind<-c(1,3,7,14)
M4.ind<-c(2,7,13,14)

MSE.M3<-rep(0,5)
MSE.M4<-rep(0,5)

for(i in 1:5){
  fold<-unlist(folds[i])
  xnew_train<-x[-fold,]
  ynew_train<-y[-fold]
  xnew_test<-x[fold,]
  ynew_test<-y[fold]
  
  M3.mod<-lm(ynew_train~.,data = xnew_train[,M3.ind])
  M4.mod<-lm(ynew_train~.,data = xnew_train[,M4.ind])
  
  MSE.M3[i]<-sum((predict(M3.mod,xnew_test[,M3.ind])-ynew_test)^2)/length(ynew_test)
  MSE.M4[i]<-sum((predict(M4.mod,xnew_test[,M4.ind])-ynew_test)^2)/length(ynew_test)
}

ARMSE.M3<-sqrt(sum(MSE.M3))/5
ARMSE.M4<-sqrt(sum(MSE.M4))/5
ARMSE.M3
ARMSE.M4
#-------------------------------------
M_final<-M4
library(boot)
library(bootstrap)

res<-M_final$residuals

bootstrap_func<-function(data,indices){
  
  bootstrap_res<-res[indices]
  
  bootstrap_response <- brozek + bootstrap_res
  bootstrap_model <- lm(bootstrap_response ~., data = x[M4.ind])
  return(coef(bootstrap_model))
}

results1<-boot(data=res,statistic = bootstrap_func, R=1000)
#results1
#summary(M_final)
bootstrap_coefficients <- colMeans(results1$t)
print(bootstrap_coefficients)
print(M4$coefficients)

bootstrap_se <- apply(results1$t, 2, sd)
print(bootstrap_se)
print(summary(M4)$coefficients[,"Std. Error"])
