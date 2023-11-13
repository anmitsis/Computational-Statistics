library(MASS)
data(mcycle)
y<-mcycle$accel
x<-mcycle$times

kernel_funct<-function(xi,xj,h){
  x<-(xi-xj)/h
  return(exp(-0.5*x^2)/sqrt(2*pi))
}

likelihood<-function(x,h){
  n<-length(x)
  kernel_sum<-rep(0,n)
  for( i in 1:n){
    temp<-0
    for(j in x[-i]){
      temp<-temp + kernel_funct(x[i],j,h)
    }
    kernel_sum[i]<-temp
  }
  #return(sum(-n*log((n-1)*h)+log(kernel_sum)))
  return(sum(log(kernel_sum))-n*log((n-1)*h))
}

h<-seq(from = 1, to = 5, by = 0.01)
length(h)
likelihood_vec<-rep(0,length(h))

for(k in 1:length(h)){
  likelihood_vec[k]<-likelihood(x,h[k])
}
plot(h,likelihood_vec,xlab = "hh")
h_opt<-h[which.max(likelihood_vec)]
plot(density(x,kernel = "gaussian", bw=h_opt),main = "Density estimation")
