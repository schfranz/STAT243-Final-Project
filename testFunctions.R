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