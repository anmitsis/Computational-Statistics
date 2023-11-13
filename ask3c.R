library(mvtnorm) 
library(MASS)
data<-unname(as.matrix(read.csv2("D:\\downloads\\datafile.csv")))
#-----------------------------------------------------

EM <- function(data, tolerance = 1e-6){
 
  n <- nrow(data)  
  d <- ncol(data)  
  
  #m1<-c(-8,-10)
  #m2<-c(8,5)
  #S1<-matrix(c(10,2,2,9),nrow=2)
  #S2<-matrix(c(1,0,0,1),nrow=2)
  m1<-c(-1,-2)
  m2<-c(1.5,1)
  S1<-matrix(c(0.7,1,1,3),nrow = 2)
  S2<-matrix(c(1.8,0.5,0.5,1),nrow = 2)
  lambda<-0.4
  
  converged <- FALSE
  old.params <- c(lambda, m1, as.vector(S1), m2, as.vector(S2))
  params.history <- list()
  
  while (!converged){
    
    params.history[[length(params.history) + 1]] <- list("lambda" = lambda, "m1" = m1, "S1" = S1, "m2" = m2, "S2" = S2)
   
    prob1 <- lambda * dmvnorm(data, mean = m1, sigma = S1,log = TRUE)  #log likelihoods
    prob2 <- (1 - lambda) * dmvnorm(data, mean = m2, sigma = S2,log = TRUE)
    #prodprob1<-prod(prob1)
    #prodprob2<-prod(prob2)
    tau1 <- prob1 / (prob1 + prob2) #responsibilities
    tau2 <- 1-tau1
    
    # M-Step: update the parameters to maximize the expected log-likelihood
    #lambda <- sum(tau1)/n  
    lambda<-mean(tau1)
    m1 <- colSums(tau1 * data) / sum(tau1)  
    m2 <- colSums(tau2 * data) / sum(tau2) 
    S1 <- t(tau1 * (data - m1)) %*% (data - m1) / sum(tau1)  
    S2 <- t(tau2 * (data - m2)) %*% (data - m2) / sum(tau2)  
   #S1 <- t((data - m1)) %*% (tau1*(data - m1)) / sum(tau1)
   # S2 <- t((data - m2)) %*% (tau2*(data - m2)) / sum(tau2)
    
    new.params <- c(lambda, m1, as.vector(S1), m2, as.vector(S2))
    param.diff <- sum(abs(new.params - old.params))
    if (param.diff < tolerance){
      converged <- TRUE
    }
    old.params <- new.params
  }
  
 
  return(list("final" = list("lambda" = lambda, "m1" = m1, "S1" = S1, "m2" = m2, "S2" = S2), "history" = params.history))
}


results<-EM(data)
results$final
length(results$history)
#for(i in 1:10){
 # print(paste("Iteration", i))
  #print(results$history[[i]])
#}

#-------------------------------------------------------------------------------------------------

N=3000
U =runif(N)
rand.samples = matrix(NA,nrow = N,ncol=2)

for(i in 1:N){
  if(U[i]<results$final[[1]]){
    rand.samples[i,] = mvrnorm(1,results$final[[2]],S1)#results$final[[3]]
  }else{
    rand.samples[i,] = mvrnorm(1,results$final[[4]],S2)#results$final[[5]]
  }
}
plot(rand.samples,xlim = c(-4,7),ylim = c(-9,4))

