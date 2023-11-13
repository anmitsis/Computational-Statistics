library(MASS)
data(mcycle)
y<-mcycle$accel
x<-mcycle$times

h<-seq(0.01,5,0.01)
cv_mse<-rep(0,length(h))
n<-length(x)

for( i in 1:length(h)){
  total_mse<-0
  
  for(j in 1:n){
    x_train<-x[-j]
    y_train<-y[-j]
    x_test<-x[j]
    y_test<-y[j]
    
    w<-(1/sqrt(2*pi))*exp(-0.5*((x_train-x_test)/h[i])^2)
    estimate<-sum(w*y_train)/sum(w)
    
    mse<-(y_test-estimate)^2
    total_mse<-total_mse + mse
  }
  cv_mse[i]<-total_mse/n
}

min_mse<-which.min(cv_mse)
min_mse
h_opt<-h[min_mse]

cv_mse
plot(h,cv_mse)

pred_values<-rep(0,length(x))
variance<-rep(0,length(x))

for( i in 1:length(x)){
  weights<-(1/sqrt(2*pi))*exp(-0.5*((x-x[i])/h_opt)^2)
  
  pred_values[i]<-sum(weights*y)/sum(weights)
  variance[i]<-sum(weights*(y-pred_values[i])^2)/sum(weights)
}         

plot(x,y,main="Nadaraya-Watson Regression",xlab = "Times",ylab = "Acceleration")
lines(x,pred_values,col=5,lw=3)
lines(x,pred_values+sqrt(variance),col=2,lty=2)
lines(x,pred_values-sqrt(variance),col=2,lty=2)
legend("bottomright",legend = c("Nadaraya-Watson estimate","Variance"),col=c(5,2),lty = c(1,2),lwd = 2)
