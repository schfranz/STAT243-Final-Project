#main adaptive rejection sampling function
#inputs: 
# -G(g?)			original function
# -n					number of samples(?)
# -lb					lower bound on x axis(?)
# -ub					upper bound on x axis(?)
# -mode				mode of g (useless?)
# -batchSize	number of seeds for inverse CDF
library(assertthat)

ars <- function(g, n, lb=-Inf, ub=Inf, batchSize=100){
  
	print(nargs())
	
  # check the inputs
	print(assert_that(nargs() >= 2, msg = "Not enough input arguments")) #this prints TRUE for an empty function call??
	assert_that(nargs() <= 5, msg = "Too many input arguments")
	assert_that(is.function(g), msg = "ERROR: g is not a function, try different input.")
	assert_that(is.numeric(n)&see_if(n>0), msg = "ERROR: n must be a valid number of sample size.")
  assert_that(see_if(lb<ub), msg = "ERROR: 'lb' must be smaller than ub, try different bounds.")
  
  #find starting xk
  xk <- initialization_step(h, lb, ub)
  
  #initialize output variable
  newSample <- NULL
  
  #iterate until sample size is satisfied
  while(length(newSample) < n){
    
    # calculate hk and derivative of hk
    hk <- h(xk)
    dhk <- sapply(xk,cal_grad,h)
    
    #intersection points of upper envelope
    zk <- generate_intersect(hk,dhk,xk,lb,ub)
    
    #cumulative envelop 
    #Calculate areas under exponential upper bound function for normalization purposes
    portion <-  unlist(sapply(2:length(zk),
                              function(i){integrate(Vectorize(exp_upper_hull,vectorize.args =c("x")),
                                                    zk[i-1],zk[i],hk,dhk,xk,zk)})[1,])
    cum <- sum(portion)
    
    # Normalize and cumulation
    envelop <- portion/cum
    cumEnv <- cumsum(envelop)
    cumEnv <- c(0,cumEnv)
    
    # Sampling: Generate seeds for Inverse CDF method
    # Generate random seeds
    seeds <- runif(batchSize)
    xSample <- sapply(seeds, draw_sample,
                      cumEnv = cumEnv, hk = hk,
                      xk = xk, dhk = dhk,
                      zk = zk,portion = portion)
    
    #Rejection testing
    testResult <- sapply(xSample, rejection_test,
                         hk = hk, xk = xk,
                         dhk = dhk, zk = zk)
    
    keepSample <- testResult[1,]
    add2xk <- testResult[2,]
    
    # extract index of kept samples
    # update accpeted points to sample points
    keepX <- xSample[keepSample > 0]
    newSample <- c(xKeep, newSample)
    
    # update xk
    # sort xk for the next iteration
    xkUpdated = xSample[add2xk > 0]
    xk = sort(c(xk, xkUpdated))
    
    
  }
  return(newSample[1:n])
}