#supporting functions (might want to make one .R script per function in package)

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
  maxPoint <- optim(par=0, f = h, method = "L-BFGS-B",
                    lower = lb, upper = ub, control=list(fnscale=-1))$par
  if (lb==-Inf & ub==Inf){
    leftPoint = maxPoint-1
    rightPoint = maxPoint +1
    midPoint = maxPoint
  }
  else if (lb == -Inf & ub!=Inf){
    if (abs(maxPoint - ub)<1e-3){
      leftPoint = maxPoint-1
      rightPoint = maxPoint
      midPoint = maxPoint-1/2
    }
    else{
      delta = abs(maxPoint-ub)
      leftPoint = maxPoint-delta/2
      rightPoint = maxPoint+delta/2
      midPoint = maxPoint
    }
  }
  else if (lb != -Inf & ub == Inf){
    if (abs(maxPoint - lb)<1e-3){
      leftPoint = maxPoint
      rightPoint = maxPoint+1
      midPoint = maxPoint+1/2
    }
    else{
      delta = abs(maxPoint-lb)
      leftPoint = maxPoint-delta/2
      rightPoint = maxPoint+delta/2
      midPoint = maxPoint
    }
  }
  else{
    delta <- (ub - lb)/2
    #taking care of exp case
    if (abs(maxPoint - ub) < 1e-3) {
      rightPoint <- maxPoint
      midPoint <- maxPoint - .5*delta
      left_point <- max - delta
    } else if (abs(maxPoint - lb) < 1e-3) {
      rightPoint <- maxPoint + delta
      midPoint <- maxPoint + .5*delta
      leftPoint <- maxPoint
    } else {
      rightPoint <- maxPoint + .5*delta
      leftPoint <- maxPoint - .5*delta
      midPoint <- maxPoint
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
rejection_test <- function(x, h, hk, dhk, xk, zk){

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
