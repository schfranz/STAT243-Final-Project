#unit tests for supporting functions

#variables needed for tests
g <- function(x) dnorm(x,0,1) #normal distribution as function input
lb <- 0 #functional lower bound
ub <- 10 #functional upper bound
deriv0 <- 0 #functional derivative of 0
deriv1 <- 1 #functional derivative of 1

#test cal_grad()
testthat::test_that("cal_grad() returns an appropriate derivative", {
	testthat::expect_equal(cal_grad(0, g), 0)
	testthat::expect_equal(cal_grad(0.5*pi, sin), 0)
})

#test generate_intersect()
testthat::test_that("generate_intersect returns appropriate intersects between points with known derivatives and has lower and upper bound as first and last values", {
	testthat::expect_equal(generate_intersect(1, deriv0, 2, lb, ub), c(lb, ub))
	testthat::expect_equal(generate_intersect(c(lb, lb+1), c(deriv0, deriv0), c(ub-1, ub), lb, ub), c(lb, Inf, ub))
	testthat::expect_equal(generate_intersect(c(lb, lb+1), c(deriv0, deriv1), c(ub-1, ub), lb, ub), c(lb, ub-1, ub))
})

#test initialization_step()


#test upper_hull()


#test exp_upper_hull()


#test lower_hull()


#test draw_sample()


#test rejection_test()


#test is.wholenumber()
test_that("is.wholenumber() returns FALSE for non-integers", {
	expect_equal(is.wholenumber(2.3), FALSE)
	expect_equal(is.wholenumber(4), TRUE)
	expect_equal(is.wholenumber(3.0003, tol = 0.0001), FALSE)
	expect_equal(is.wholenumber(3.0003, tol = 0.001), TRUE)
})


#test check_f_positive()


#test check_concave()


