#devtools::use_testthat()

Sys.setenv("R_TESTS" = "")
library(testthat)
#library(noarr) trying this

test_check("noarr")
