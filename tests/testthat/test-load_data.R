#library(noarr) trying this
library(testthat)

test_that("Data can be pulled in raw form and loaded into memory", {
  df <- load_data()
  expect_equal(dim(df)[1], 6207)
  expect_true("data.frame" %in% class(df))
})
