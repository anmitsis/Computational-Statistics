library("MASS")
data<-unname(as.matrix(read.csv2("C:\\Users\\Antonis\\Downloads\\datafile.csv")))
means<-c(mean(data[,1]),mean(data[,2]))
n<-nrow(data)
s11<-sum((data[,1]-means[1])^2)/n
s22<-sum((data[,2]-means[2])^2)/n
s12<-sum((data[,1]-means[1])*(data[,2]-means[2]))/n
cov_matrix<-matrix(c(s11,s12,s12,s22),nrow = 2)
new_data <- mvrnorm(n = n, mu = means, Sigma = cov_matrix)
plot(new_data)
#---------------------------------------------------------------------------------
means
