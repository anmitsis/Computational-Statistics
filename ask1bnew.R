ask1b_trace <- function(d, nreps = 10000, c) {
  states <- matrix(0, nrow = nreps, ncol = d)
  states[1,] <- runif(d, -1, 1)
  accepted = 0
  
  dims <- 1:(d-1)
  
  for (i in 2:nreps) {
    prop_state <- states[i - 1, ]
    
    dim <- sample(dims, 1)
    prop_state[dim] <- prop_state[dim] + runif(1, -c, c)
    
    while (sum(prop_state[1:(d-1)]^2) > 1) {
      prop_state[dim] <- prop_state[dim] + runif(1, -c, c)
    }
    
    prop_state[d] <- runif(1,-1,1)
    #prop_state[d] <- min(max(prop_state[d], -1), 1)
    
    if (sum(prop_state^2) <= 1) {
      accepted <- accepted + 1
    } 
    states[i, ] <- prop_state
  }
  
  list(states = states, acceptance_rate = accepted / nreps)
}
trace_result <- ask1b_trace(3, 10000, 0.1)
scatterplot3d(trace_result$states[, 1], trace_result$states[, 2], trace_result$states[, 3], 
              main="3D Trace Plot", xlab="Dim 1", ylab="Dim 2", zlab="Dim 3")
trace_result$acceptance_rate
