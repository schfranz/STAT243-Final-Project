library(numDeriv)
#construct zk
generate_zk <- function(x,h,lb,ub){
  k = length(x)
  x_j1 <- c(0,x)
  x_j <- c(x,0)
  zk <- (h(x_j1)-h(x_j)-x_j1*grad(h,x_j1)+x_j*grad(h,x_j))/(grad(h,x_j)-grad(h,x_j1))
  tmp <- zk
  tmp[1] <- lb
  tmp[length(x)+1] <- ub
  return(tmp)
}


#find two starting x_k
find_two_points <- function(h, lb, ub){
  delta <- (ub - lb)/500
  max <- optimize(f = h, interval = c(lb, ub), lower = lb, upper = ub, maximum = TRUE)$maximum
  #taking care of exp case
  if (abs(max - ub) < .0001) {
    right_point <- max
    mid <- max - .5*delta
    left_point <- (max - delta)
    X_init <- c(left_point,mid,right_point)
  } else if (abs(max - lb) < .0001) {
    right_point <- (max + delta)
    mid <- max + .5*delta
    left_point <- max
    X_init <- c(left_point,right_point)
  } else {
    right_point <- (max + delta)
    left_point <- (max - delta)
    X_init <- c(left_point,max,right_point)
  }
  return(X_init)
}


#find lower bound for certain point
lower_function <- function(x,h_k,dh_k,x_k){
  if(x < min(x_k) | x > max(x_k)){
    return(-Inf)
  }else{
    i <- max(which(x >= x_k))
    lower_func <- ((x_k[i + 1] - x) * x_k[i] + (x - x_k[i]) * h_k[i + 1])/(x_k[i + 1] - x_k[i])
    return(lower_func)
  
  }
}

#find upper bound for certain point. And exponential of it.
upper_function <- function(x,h_k,dh_k,x_k,z_k){
  i <- min(which(x < z_k)-1)
  upper_func <- h_k[i] + (x - x_k[i]) * dh_k[i]
  return(upper_func)
}

exp_upper <- function(x,h_k,dh_k,x_k,z_k){
  exp_up <- exp(upper_function(x,h_k,dh_k,x_k,z_k))
  return(exp_up)
}

#drawing sample from the envelope
generate_sample <- function(u,cum_env,h_k,x_k,dh_k,z_k,area){
  j <- max(which(u > cum_env))
  if(dh_k[j] == 0){
    x <- runif(1, z_k[j],z_k[j+1])
    return(x)
  }else{
    
    # Sample from uniform random
    w = runif(1)
    
    # Rescale sample value w to area of the selected segment, since area under segment is not equal to 1
    w_sc = w*(1/area[j])*exp(h_k[j] - x_k[j]*dh_k[j])*(exp(dh_k[j]*z_k[j+1]) -exp(dh_k[j]*z_k[j]))
    
    # Use inverse CDF of selected segment to generate a sample
    x = (1/dh_k[j])*log(w_sc*area[j]/(exp(h_k[j] - x_k[j]*dh_k[j])) + exp(z_k[j]*dh_k[j]))
  }
  return(x)
}
#rejection test
rej_test <- function(x, h_k, dh_k, x_k, z_k){
  # Generate random seed
  w = runif(1)
  
  # squeeze and reject tests indicator for adding point in boolean form
  squeeze = FALSE
  accept = FALSE
  add = FALSE
  
  # get rejection point for squeeze and accept test
  lower_test = exp(lower_function(x,h_k,dh_k,x_k) - upper_function(x,h_k,dh_k,x_k,z_k))
  function_test = exp(h(x) - upper_function(x,h_k,dh_k,x_k,z_k))
  
  
  if( w <= lower_test){
    
    squeeze = TRUE
    accept = TRUE
    
  }else if(w <= function_test){
    
    squeeze = FALSE
    accept = TRUE
    
  }else{
    
    accept = FALSE
  }
  
  # Determine whether to add point to abscissae
  if(squeeze * accept == FALSE) add = TRUE
  
  # Return boolean indicating whether to accept candidate sample point
  return(list(rej = squeeze + accept,add = add))
}



ars <- function(G, n, lb, ub, mode, groupsize){
  
  #log of the original function  
  h <- function(x){
    return(log(g(x)))
  }
  #find starting x_k
  x_k <- find_two_points(h, lb, ub)
  
  #initialize output variable
  new_sample <- NULL
  
  #iterate until we have enough points
  while(length(new_sample) < n){
    
    #intersection points
    z_k <- generate_zk(x_k,h,lb,ub)
    #h_k & h_k'
    h_k <- h(x_k)
    dh_k <- grad(h, x_k)
    
    #cumulative envelop 
      #Calculate areas under exponential upper bound function for normalization purposes
    area <-  unlist(sapply(2:length(z_k),function(i){integrate(exp_upper,z_k[i-1],z_k[i],h_k,dh_k,x_k,z_k)})[1,])
    cum <- sum(area)
      #Normalize
    envelop <- area/cum
    cum_env <- cumsum(envelop)
    cum_env <- c(0,cum_env)
    
    #Sampling: Generate seeds for Inverse CDF method
    seeds <- runif(groupsize)
    x_sample <- sapply(seeds, generate_sample,
                       cum_env = cum_env,
                       h_k = h_k,
                       x_k = x_k,
                       dh_k = dh_k,
                       z_k = z_k,
                       area = area)
    
    #Rejection testing
    test_result <- sapply(x_sample, rej_test,
                          h_k = h_k,
                          x_k = x_k,
                          dh_k = dh_k,
                          z_k = z_k)
    keep_sample <- test_result[1,]
    add_to_x_k <- test_result[2,]
    
    # update accpeted points to sample
    x_keep <- x_sample[keep_sample > 0]
    new_sample <- c(x_keep, new_sample)
    
    # update x_k
    x_k_updated = x_sample[add_to_x_k > 0]
    x_k = sort(c(x_k, x_k_updated))
    
    
  }
  return(new_sample)
}


#sample test
g <- function(x) dnorm(x,0,1)
x <- ars(g,5000,-10,10,0,groupsize=100)

