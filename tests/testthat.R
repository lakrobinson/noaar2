#devtools::use_testthat()

Sys.setenv("R_TESTS" = "")
library(testthat)
library(noarr)

test_check("noarr")
