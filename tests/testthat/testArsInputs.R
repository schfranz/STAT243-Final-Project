#tests for different variations of number of inputs and input types

#necessary variables
g <- function(x) dnorm(x,0,1) #functional sample input for argument g
n <- 5000 #functional sample input for argument n
gBroken <- runif(100) #broken sample input for argument g
nBroken1 <- seq_len(3) #vector input for n
nBroken2 <- 0.3 #non-integer input for n


#faulty function calls -- number of inputs MOEP tests don't work because ars() doesn't use assert_that with nargs??
test_that("ars() can't be called with wrong number of inputs", {
	expect_error(ars(), "Not enough input arguments")
	expect_error(ars(g), "Not enough input arguments")
	expect_error(ars(g, n, 0, 5, 100, 3), "Too many input arguments")
})

#acceptable function calls -- number of inputs
#test_that("ars() runs properly with correct number of inputs", {
#	expec
#})
#ars(g, 3, 0, 10)


#faulty function calls -- format of inputs
test_that("ars() cannot be called with inputs of the wrong format", {
	expect_error(ars(gBroken, n), "not a function")
	expect_error(ars(g, nBroken1), "")
})


#faulty function calls -- logical input errors