#unit tests for supporting functions


#test generate_intersect()


#test initialization_step()


#test upper_hull()


#test exp_upper_hull()


#test lower_hull()


#test draw_sample()


#test rejection_test()


#test is.wholenumber()
testthat::test_that("is.wholenumber() returns FALSE for non-integers", {
  testthat::expect_equal(is.wholenumber(2.3), FALSE)
  testthat::expect_equal(is.wholenumber(4), TRUE)
  testthat::expect_equal(is.wholenumber(3.0003, tol = 0.0001), FALSE)
  testthat::expect_equal(is.wholenumber(3.0003, tol = 0.001), TRUE)
})


#test check_f_positive()


#test cal_grad()


#test check_concave()


