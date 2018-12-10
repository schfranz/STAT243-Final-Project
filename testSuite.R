#test suite
#execute to see if function passes all tests

# assert normal density
assertthat::assert_that(check_f_positive(dnorm,-100,100))


#sample test
h <- function(x){
	return(log(g(x)))
}
g <- function(x) dnorm(x,0,1)
x <- ars(g,5000,-10,10,0,batch_size=100)