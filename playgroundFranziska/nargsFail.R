#mysterious R bug
print(nargs()) #prints 0
print(nargs() > 1) #prints FALSE
na <- nargs()
print(na)

print(missing(g))

# check the inputs
print(assert_that(see_if(nargs() >= 2), msg = "Not enough input arguments")) #this prints TRUE for an empty function call??
assert_that(nargs() > 1, msg = "Not enough input arguments")
assert_that(na > 1, msg = "What in the")
