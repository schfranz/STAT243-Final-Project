#TODO: determine way to automatically set working directory to location of this file

#source other scripts to load all necessary functions into the workspace; TODO: fail more gracefully if files aren't present
source("arsFunction.R") #, local = TRUE, chdir = TRUE) <- other options for source() that could potentially help automatically setting working directory
source("supportingFunctions.R")
source("testFunctions.R")
source("testSuite.R")
