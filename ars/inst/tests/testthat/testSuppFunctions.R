#unit tests for supporting functions

#variables needed for tests
g <- function(x) dnorm(x,0,1) #normal distribution as function input

#test cal_grad()
testthat::test_that("cal_grad() returns an appropriate derivative", {
	expect_equal(cal_grad(0, g), 0)
	expect_equal(cal_grad(0.5*pi, sin), 0)
})

#test generate_intersect()


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


