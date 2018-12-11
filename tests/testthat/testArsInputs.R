#tests for different variations of number of inputs and input types

#faulty function calls
test_that("ars() can't be called with wrong number of inputs", {
	expect_error(ars(), "not enough input arguments")
	expect_error()
})


#acceptable function calls
g <- function(x) dnorm(x,0,1)
ars(g, 3, 0, 10)