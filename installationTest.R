#check that package can be installed and runs properly

#get package from github; should print a bunch of stuff
devtools::install_github('schfranz/STAT243-Final-Project/ars')

#make package available
library(ars)

#load testing library
library(testthat)

#see if ars() is visible and throws the expected error
test_that("ars() can't be called with wrong number of inputs", {
	expect_error(ars(), "Missing input arguments")
})

#do a proper run with ars() that should work
g <- function(x) dnorm(x,0,1)
ars(g, 5000, 2, 6, 100)

#test that all tests are passed
#note: this requires the working directory to be set to the package directory
#run either devtools::test() for a brief overview
#or run testthat::test_dir("tests/") for a detailed test with information about individual test outcomes