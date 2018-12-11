#supporting functions (might want to make one .R script per function in package)

h <- function(x){
  return(log(g(x)))
}

# generate intersect zj
generate_intersect <- function(hk, dhk, xk, lb, ub){
  # length of hk
  k = length(hk)
  # various vectors
  xkl <- c(xk,0); xko <- c(0,xk)
  hkl <- c(hk,0); hko <- c(0,hk)
  dhkl <- c(dhk,0); dhko <- c(0,dhk)
  
  # compute zj
  zj <- (hkl-hko-xkl*dhkl+xko*dhko)/(dhko-dhkl)
  # set starting point and ending point
  zj[1] <- lb
  zj[k+1] <- ub
  return(zj)
}




# initialization
initialization_step <- function(h, lb, ub){
  
  # considering lb and ub is infinity
  # pre-set interval delta
  if (lb==-Inf | ub==Inf){
    maxPoint <- optim(par=0, f = h, method = "L-BFGS-B", 
                      lower = lb, upper = ub, control=list(fnscale=-1))$par
    if (lb==-Inf & ub==Inf){ 
      rightPoint <- maxPoint - 1
      midPoint <- maxPoint
      leftPoint <- maxPoint + 1
    }
    else if (lb==-Inf & ub!=Inf) {
      if (abs(maxPoint - ub) < 1e-5) {
        rightPoint <- maxPoint
        midPoint <- maxPoint - 1/2
        leftPoint <- (maxPoint - 1)
      }
      else{
        delta = (ub - maxPoint)/2
        rightPoint <- maxPoint + delta
        midPoint <- maxPoint
        leftPoint <- (maxPoint - delta)
      }
    }
    else if (lb!=-Inf & ub==Inf) {
      if (abs(maxPoint - lb) < 1e-5) {
        rightPoint <- maxPoint + 1
        midPoint <- maxPoint + 1/2
        leftPoint <- maxPoint
      }
      else{
        delta = (maxPoint - lb)/2
        rightPoint <- maxPoint + delta
        midPoint <- maxPoint
        leftPoint <- (maxPoint - delta)
      }
    }
  }
  else {
    maxPoint <- optimize(f = h, interval = c(lb, ub), 
                         lower = lb, upper = ub, maximum = TRUE)$maximum
    delta = (ub-lb)/2
    # set three points
    if (abs(maxPoint - ub) < 1e-5) {
      rightPoint <- maxPoint
      midPoint <- maxPoint - delta/2
      leftPoint <- (maxPoint - delta)
    } else if (abs(maxPoint - lb) < 1e-5) {
      rightPoint <- (maxPoint + delta)
      midPoint <- maxPoint + delta/2
      leftPoint <- maxPoint
    } else {
      rightPoint <- (maxPoint + delta)
      midPoint <- maxPoint
      leftPoint <- (maxPoint - delta)
    }
  }
  
  init <- c(leftPoint, midPoint, rightPoint)
  return(init)
}


# create upper hull in a vectorized fashion
upper_hull <- function(x,hk,dhk,xk,zk){
  i <- min(which(x < zk)-1)
  upperBound <- hk[i] + (x - xk[i]) * dhk[i]
  return(upperBound)
}

# take exponential values of upper hull
exp_upper_hull <- function(x,hk,dhk,xk,zk){
  expUpBound <- exp(upper_hull(x,hk,dhk,xk,zk))
  return(expUpBound)
}


# create lower hull in a vectorized fashion
lower_hull <- function(x,hk,dhk,xk){
  if(x < min(xk) | x > max(xk)){
    return(-Inf)
  }else{
    i <- max(which(x >= xk))
    lowerBound <- ((xk[i + 1] - x) * xk[i] + (x - xk[i]) * hk[i + 1])/(xk[i + 1] - xk[i])
    return(lowerBound)
  }
}


# sample from the envelope
draw_sample <- function(u, cumEnv, hk, xk, dhk, zk, portion){
  
  j <- max(which(u > cumEnv))
  
  if(dhk[j] == 0){
    x <- runif(1, zk[j],zk[j+1])
    return(x)
  } else {
    
    # Sample from uniform random
    w = runif(1)
    
    # Rescale sample value w to area of the selected segment
    wRescale = w/portion[j]*exp(hk[j] - xk[j]*dhk[j])*(exp(dhk[j]*zk[j+1]) -exp(dhk[j]*zk[j]))
    
    # Use inverse CDF of selected segment to generate a sample
    x = (1/dhk[j])*log(wRescale*portion[j]/(exp(hk[j] - xk[j]*dhk[j])) + exp(zk[j]*dhk[j]))
  }
  return(x)
}

# adaptive rejection test
rejection_test <- function(x, hk, dhk, xk, zk){
  
  # Generate random sample from uniform distribution
  w = runif(1)
  
  # squeeze and reject tests indicator for adding point in boolean form
  accept = FALSE
  add = FALSE
  
  # get rejection point for squeeze and accept test
  lowerTest = exp(lower_hull(x,hk,dhk,xk) - upper_hull(x,hk,dhk,xk,zk))
  
  # check if we need to keep sample points
  # check if we need to add new points to xk
  # kernel test
  if(w <= lowerTest){
    accept = TRUE
  } else {
    add = TRUE
    functionTest = exp(h(x) - upper_hull(x,hk,dhk,xk,zk))
    if(w <= functionTest){
      accept = TRUE
    }else{
      accept = FALSE
    }
  }
  
  # Return boolean indicator whether to accept candidate sample point
  # and update these points to xk
  return(list(acceptIndicator = accept , UpdateIndicator = add))
}