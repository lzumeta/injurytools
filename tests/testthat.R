Sys.setenv("R_TESTS" = "")
library(testthat)
library(injurytools)

test_check("injurytools")
