### Adaptive Rejection Sampling ###


# check whether f is positive in range from var_lower to var_upper

library(rootSolve)

# f is continuous
check_f_positive = function(f, lower, upper) {
  # choose a test point in interval
  # may be more efficient
  test_point = min(upper, lower+1)
  if (f(test_point) < 0) {
    return(FALSE)
  }
  
  # root
  f_lower <- f(lower)
  f_upper <- f(upper)
  # if the sign of boundary values differ
  if(sign(f_lower) != sign(f_upper)){
    return(FALSE)
  }
  
  roots <- try(uniroot.all(Vectorize(f), lower = lower, upper = upper)) 
  # if run error
  if(class(roots)=="try-error"){
    stop("Error in uniroot.all.")
  }
  
  # if no root in interval
  if(length(roots)==0){
    if(f_lower>0) return(TRUE)
    if(f_lower <= 0) return(FALSE)
  }
  else{
    root_p = roots + 1e-5
    root_m = roots - 1e-5
    if((sum(f(root_p)<0) == 0) & (sum(f(root_m)<0) == 0)){
      return(TRUE)
    }
    return(FALSE)
  }
}

# assert normal density
assertthat::assert_that(check_f_positive(dnorm,-100,100))

# calculate derivative of a function
# instead of "grad"
cal_grad = function(x, f, lower=x-0.001, upper=x+0.001, ...) {
  eps <- (.Machine$double.eps)^(1/4)
  d <- numeric(0)
  if (x==lower) d <- (f(x + eps, ...) - f(x, ...))/eps
  else if (x==upper) d <-  (f(x, ...)- f (x - eps, ...))/eps
  else if (lower <= x && x <= upper) d <- (f(x + eps, ...)-f(x - eps, ...))/(2*eps)
  else stop("x is out of bounds.")
  
  # if limit doesn't exist then we need to stop
  if(is.na(d) | is.infinite(d) | is.nan(d)) stop("The derivative does not exist.")
  return(d)
}


# check h(x) is concave
check_concave = function(x, h) {
  if (length(x) != length(h)) {
    stop("x and h should be of equal length.")
  }
  concave = TRUE
  for (i in 1:(length(x)-2)) {
    inter = h[i] + (x[i+1] - x[i]) * (h[i+2] - h[i])/(x[i+2] - x[i]) 
    if (inter > h[i+1]) {
      concave = FALSE
      break
    }
  }
  return(concave)
}

h <- function(x){
  return(log(g(x)))
}

# main function starts here

library(numDeriv)

h <- function(x){
  return(log(g(x)))
}

# generate intersect z_j
generate_intersect <- function(h_k, dh_k, x_k, lb, ub){
  k = length(h_k)
  x_k_l <- c(x_k,0)
  x_k_o <- c(0,x_k)
  h_k_l <- c(h_k,0)
  h_k_o <- c(0,h_k)
  dh_k_l <- c(dh_k,0)
  dh_k_o <- c(0,dh_k)
  zj <- (h_k_l-h_k_o-x_k_l*dh_k_l+x_k_o*dh_k_o)/(dh_k_o-dh_k_l)
  zj[1] <- lb
  zj[k+1] <- ub
  return(zj)
}


# initialization
initialization_step <- function(h, lb, ub){
  delta <- (ub - lb)/500
  max_point <- optimize(f = h, interval = c(lb, ub), lower = lb, upper = ub, maximum = TRUE)$maximum

  if (abs(max_point - ub) < 1e-5) {
    right_point <- max_point
    mid_point <- max_point - delta/2
    left_point <- (max_point - delta)
  } else if (abs(max_point - lb) < 1e-5) {
    right_point <- (max_point + delta)
    mid_point <- max_point + delta/2
    left_point <- max_point
  } else {
    right_point <- (max_point + delta)
    mid_point <- max_point
    left_point <- (max_point - delta)
  }
  
  init <- c(left_point, mid_point, right_point)
  return(init)
}


# create upper hull in a vectorized fashion
# take exponential
upper_hull <- function(x,h_k,dh_k,x_k,z_k){
  i <- min(which(x < z_k)-1)
  upperBound <- h_k[i] + (x - x_k[i]) * dh_k[i]
  return(upperBound)
}

exp_upper_hull <- function(x,h_k,dh_k,x_k,z_k){
  expUpBound <- exp(upper_hull(x,h_k,dh_k,x_k,z_k))
  return(expUpBound)
}


# create lower hull in a vectorized fashion
lower_hull <- function(x,h_k,dh_k,x_k){
  if(x < min(x_k) | x > max(x_k)){
    return(-Inf)
  }else{
    i <- max(which(x >= x_k))
    lowerBound <- ((x_k[i + 1] - x) * x_k[i] + (x - x_k[i]) * h_k[i + 1])/(x_k[i + 1] - x_k[i])
    return(lowerBound)
  }
}


# sample from the envelope
draw_sample <- function(u, cum_env, h_k, x_k, dh_k, z_k, portion){
  
  j <- max(which(u > cum_env))
  
  if(dh_k[j] == 0){
    x <- runif(1, z_k[j],z_k[j+1])
    return(x)
  } else {
    
    # Sample from uniform random
    w = runif(1)
    
    # Rescale sample value w to area of the selected segment, since area under segment is not equal to 1
    w_sc = w/portion[j]*exp(h_k[j] - x_k[j]*dh_k[j])*(exp(dh_k[j]*z_k[j+1]) -exp(dh_k[j]*z_k[j]))
    
    # Use inverse CDF of selected segment to generate a sample
    x = (1/dh_k[j])*log(w_sc*portion[j]/(exp(h_k[j] - x_k[j]*dh_k[j])) + exp(z_k[j]*dh_k[j]))
  }
  return(x)
}

#rejection test
rejection_test <- function(x, h_k, dh_k, x_k, z_k){
  # Generate random seed
  w = runif(1)
  
  # squeeze and reject tests indicator for adding point in boolean form
  accept = FALSE
  add = FALSE
  
  # get rejection point for squeeze and accept test
  lower_test = exp(lower_hull(x,h_k,dh_k,x_k) - upper_hull(x,h_k,dh_k,x_k,z_k))
  
  if(w <= lower_test){
    accept = TRUE
    } else {
      add = TRUE
      function_test = exp(h(x) - upper_hull(x,h_k,dh_k,x_k,z_k))
      if(w <= function_test){
        accept = TRUE
        }else{
          accept = FALSE
        }
      }
  
  # Return boolean indicator whether to accept candidate sample point
  return(list(acceptIndicator = accept , UpdateIndicator = add))
}



ars <- function(G, n, lb, ub, mode, batch_size){
  
  #log of the original function  
  h <- function(x){
    return(log(g(x)))
  }
  #find starting x_k
  x_k <- initialization_step(h, lb, ub)
  
  #initialize output variable
  new_sample <- NULL
  
  #iterate until we have enough points
  while(length(new_sample) < n){
    
    # calculate h_k and derivative of h_k
    h_k <- h(x_k)
    dh_k <- sapply(x_k,cal_grad,h)
    
    #intersection points
    z_k <- generate_intersect(h_k,dh_k,x_k,lb,ub)
    
    #cumulative envelop 
    #Calculate areas under exponential upper bound function for normalization purposes
    portion <-  unlist(sapply(2:length(z_k),
                           function(i){
                             integrate(Vectorize(exp_upper_hull,vectorize.args =c("x")),
                                       z_k[i-1],z_k[i],h_k,dh_k,x_k,z_k)})[1,])
    cum <- sum(portion)
    #Normalize
    envelop <- portion/cum
    cum_env <- cumsum(envelop)
    cum_env <- c(0,cum_env)
    
    #Sampling: Generate seeds for Inverse CDF method
    seeds <- runif(batch_size)
    x_sample <- sapply(seeds, draw_sample,
                       cum_env = cum_env,
                       h_k = h_k,
                       x_k = x_k,
                       dh_k = dh_k,
                       z_k = z_k,
                       portion = portion)
    
    #Rejection testing
    test_result <- sapply(x_sample, rejection_test,
                          h_k = h_k,
                          x_k = x_k,
                          dh_k = dh_k,
                          z_k = z_k)
    keep_sample <- test_result[1,]
    add_to_x_k <- test_result[2,]
    
    # update accpeted points to sample
    x_keep <- x_sample[keep_sample != 0]
    new_sample <- c(x_keep, new_sample)
    
    # update x_k
    x_k_updated = x_sample[add_to_x_k > 0]
    x_k = sort(c(x_k, x_k_updated))
    
    
  }
  return(new_sample)
}


#sample test
g <- function(x) dnorm(x,0,1)
x <- ars(g,5000,-10,10,0,batch_size=100)






