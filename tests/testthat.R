#devtools::use_testthat()

Sys.setenv("R_TESTS" = "")
library(testthat)
library(noaar)

test_check("noaar")
