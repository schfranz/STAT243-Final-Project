#test suite
#execute to see if function passes all tests

require(testthat)

# assert normal density
assertthat::assert_that(check_f_positive(dnorm,-100,100))


#sample test
g <- function(x) dnorm(x,0,1)
x <- ars(g,5000,-10,10,0,batch_size=100) #?


#test that function catches non-log-concave functions
g <- function(x) dt(x, 1) #seq(from = -10, to = 10, by = 0.1)
x <- ars(g,5000,-10,10,0,batch_size=100) #x goes through without error -> fix that

#make sure this call runs
#test_package('ars')