#main adaptive rejection sampling function
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