#tests for correctness of output given known input distributions
test_that("unit test for normal distribution", {
	
	f <- dnorm
	
	samples <- ars(f, 1000,-Inf, Inf)
	
	p <- ks.test(samples, "pnorm")
	
	expect_gte(p$p.value, 0.1)
	
})test_that("unit test for beta(2,2) distribution", {
	
	f <- function(x) dbeta(x, 2, 2)
	
	samples <- ars(f, 1000, 0, 1)
	
	p <- ks.test(samples, 'pbeta', 2, 2)
	
	expect_gte(p$p.value, 0.1)
	
})test_that("unit test for gamma(2,2) distribution", {
	
	f <- function(x) dgamma(x, 2, 2)
	
	samples <- ars(f, 1000, 0, Inf)
	
	p <- ks.test(samples, 'pgamma', 2, 2)
	
	expect_gte(p$p.value, 0.1)
	
})