#main adaptive rejection sampling function
#inputs:
# -g			    	original function
# -n						number of samples desired
# -lb						lower bound on x axis
# -ub						upper bound on x axis
# -batchSize		number of seeds for inverse CDF
# -randomState	seed for set.Seed()

ars <- function(g, n, lb=-Inf, ub=Inf, batchSize=100, randomState=1){

  #relevant libraries
  library(assertthat)

	#check inputs
  assert_that(!missing(g), !missing(n), msg = "Missing input arguments")
  assertthat::assert_that(length(n) == 1, length(lb) == 1, length(ub) == 1, length(batchSize) == 1, msg = "Inputs for n, lb, ub, and batchSize must be single numeric values")
  assertthat::assert_that(is.function(g), msg = "g must be a function input")
  assertthat::assert_that(is.numeric(n), n > 0, is.wholenumber(n), msg = "n must be a positive integer value")
  assertthat::assert_that(is.numeric(lb), is.numeric(ub), msg = "upper and lower bound must be numeric values")
  assertthat::assert_that(lb < ub, msg = "Lower bound must be smaller than upper bound")
  assertthat::assert_that(is.numeric(batchSize), batchSize > 0, is.wholenumber(batchSize), msg = "batchSize must be a positive integer value")
  assertthat::assert_that(batchSize < n, msg = "batchSize must be larger than number of samples n")

	#set random seed
	set.seed(randomState)
	
	# take log
	h <- function(x){
		return(log(g(x)))
	}

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
    
    #cumulative envelope
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
  
    # drop nan
    # not affect random
    xSample <- na.omit(xSample)
    
    #Rejection testing
    testResult <- sapply(xSample, rejection_test,
                         h = h, hk = hk, xk = xk,
                         dhk = dhk, zk = zk)

    keepSample <- testResult[1,]
    add2xk <- testResult[2,]

    # extract index of kept samples
    # update accpeted points to sample points
    keepX <- xSample[keepSample > 0]
    newSample <- c(keepX, newSample)

    # update xk
    # sort xk for the next iteration
    xkUpdated = xSample[add2xk > 0]
    xk = sort(c(xk, xkUpdated))
  }
  return(newSample[1:n])
}