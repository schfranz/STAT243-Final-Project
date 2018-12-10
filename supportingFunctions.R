#supporting functions (might want to make one .R script per function in package)
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