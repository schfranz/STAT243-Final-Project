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



