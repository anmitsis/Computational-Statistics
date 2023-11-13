ask1a<- function(max_d,n) {
  
  real_vol_calc<-function(d){
    if (d == 1) {
      return(2)
      } else if (d == 2) {
      return(pi)
      } else {
      return((2 * pi / (d-2 + 2)) * real_vol_calc(d - 2))
      }
  }
  
  for(d in 2:max_d){
    
    real_volume=real_vol_calc(d)
    count_inside = 0
    
    for(i in 1:n) {
      point = runif(d, min = -1, max = 1)
      if(sum(point^2) <= 1) {
        count_inside = count_inside + 1
      }
    }
  
    if(count_inside==0){
      break
    }
  }

  estimated_volume <- 2^d * (count_inside / n)
  error=abs((real_volume-estimated_volume)/real_volume)
  print(paste("D: ",d))
  print(paste("Real Volume:", real_volume))
  print(paste("Estimated Volume:", estimated_volume))
  print(paste("The percentage of error is:", round(error,5)*100,"%",sep = ""))
  
}

ask1a(6,10^5)

